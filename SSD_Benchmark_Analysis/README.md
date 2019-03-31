# Setting Up the Machines

`sudo` is required for the setup steps below.

## Dependencies

### Installing Java

`Java8` is required for `Cassandra` as well as `YCSB`. To install:

```
add-apt-repository ppa:webupd8team/java # add the oracle java8 repository
apt update # update the package list
apt install oracle-java8-installer # install oracle java8
```

### Installing Maven

In order to run the `YCSB` benchmark for `RocksDB`, we have to compile it ourselves using `maven`. To install:

```
apt update # update the package list
apt install maven # install maven
```

### Installing YCSB

`YCSB` provides a precompiled binary that we can directly download and run. However, it does not come with the necessary binaries for RocksDB, so we have to compile those ourselves.

`RocksDB` is an embedded database, meaning it does not follow the server client architecture, but rather the database is embedded within the application itself. The `YCSB` benchmark provides such an application that we have to compile directly on the "server" machine (meaning the machine you want to store the database on). To install:

```
git clone https://github.com/brianfrankcooper/YCSB.git # clone the YCSB git repository
cd YCSB # change directory into the git repository
mvn clean package # compile YCSB
```

For `Cassandra`, simply install the precompiled binary onto the client machine. To install:

```
wget https://github.com/brianfrankcooper/YCSB/releases/download/0.15.0/ycsb-0.15.0.tar.gz # download the tar file
tar -xzf ycsb-0.15.0.tar.gz # unpack the gzipped tar file
```

## Databases

### Installing  RocksDB

If you've followed the instructions up to this point, you should have compiled `YCSB` on the "server" machine. This will have compiled everything necessary to run `YCSB` against `RocksDB` so there isn't anything else to do here.

### Installing Cassandra

At the time of writing, `Cassandra` is on version 3.11.3 and have provided precompiled binaries that we can directly run on the server machine. To install:

```
wget https://www-eu.apache.org/dist/cassandra/3.11.3/apache-cassandra-3.11.3-bin.tar.gz # download the tar file
tar -xzf apache-cassandra-3.11.3-bin.tar.gz # unpack the gzipped tar file
```

# Benchmarking

We will set up the disks and configure `YCSB` to run a benchmark against `RocksDB` and `Cassandra`.

## Unmounting

Before we get started, make sure the drives aren't mounted or we won't be able to format and mount the drives in the next step.

To check if your drive(s) has been mounted:

```
df -h
```

If you see your drive on the list (e.g. `/dev/nvme0n1`), then it is mounted, and you should see the path it is mounted at on the right most column (e.g. `/mnt`). Make sure no processes are accessing this drive, then unmount it using:

```
umount /mnt # change this to the path your drive is mounted on
```

You are now ready to get started!

## Setting Up the Drives

If your drive is named `/dev/nvme0n1`, once you've made sure that the drive has been unmounted, format the drive using one of the following 3 commands.

```
mkfs.ext4 /dev/nvme0n1 # for an ext4 file system
mkfs.f2fs /dev/nvme0n1 # for a f2fs file system
mkfs.btrfs -f /dev/nvme0n1 # for a btrfs file system
```

Then mount the drive on to the desired directory. For example, if you want to mount it at `/mnt`,

```
mount /dev/nvme0n1 /mnt
```

Once the drive has been mounted, change the ownership of the mounted directory to your current user.

```
chown -R <your uid>:<your gid> /mnt
```

Repeat this for all the necessary drives with the desired file system.

## Running blktrace

Assuming you have a drive `/dev/nvme2n1` formatted (ext4 for our purposes) and mounted at `/mnt/traces`, you can run `blktrace` on another drive `/dev/nvme0n1` as follows:

Inside of `/mnt/traces`, create a folder to contain all of the traces, and run

```
sudo blktrace -d /dev/nvme0n1 -w 80000 -o nvme0n1_trace
```

Give the folder and traces a better name for better organization.

## YCSB

`YCSB` works in two stages. The first stage populates the database with some initial data, then the second stage runs the actual workload according to the configurations.

### Breaking It Down

The two stages are seen in the example below.

```
./bin/ycsb load basic -P workloads/workloada # loading stage
./bin/ycsb run basic -P workloads/workloada # running stage
```

- `./bin/ycsb.sh` is the `YCSB` command

- `load/run` is one of the two stages. You always run `load` followed by `run` to populate the database then run the workload.

- `basic` is one of the database interface layers provided by `YCSB`, for our purposes, we are interested in `rocksdb` and `cassandra-cql`.

- `-P workloads/workloada` specifies a configuration file for the workload which we can then customize further.

## RocksDB

`RocksDB` is an embedded database, meaning it is run by embedding it into the application itself. It requires the user to specify a single directory in which to store the data. For our purposes, we will create an `ext4/f2fs/btrfs` file system on a SSD, mount the drive and tell `RocksDB` to use the root of the drive to store its data.

### Setting Up the Drives

For `RocksDB`, only a single drive is necessary for the benchmarking, but we will also use an additional drive to store the `blktrace`. Set up one drive to the desired file system, and for the `blktrace`, set up an `ext4` file system.

For the purposes of this document, I will assume the drive for `RocksDB` is `/dev/nvme0n1` and it is mounted at `/mnt/rocksdb`.

### Running RocksDB

The benchmark will startup `RocksDB` so there's nothing you have to do here.

### Running the Benchmark

```
./bin/ycsb load rocksdb -P workloads/workloada -p rocksdb.dir=/mnt/rocksdb -p recordcount=250000000 -p operationcount=1000000000 -s -threads 16
./bin/ycsb run rocksdb -P workloads/workloada -p rocksdb.dir=/mnt/rocksdb -p recordcount=250000000 -p operationcount=1000000000 -s -threads 16
```

Differences from the basic `YCSB` example from above:

- `rocksdb` is now the database interface layer rather than `basic`
- `-p rocksdb.dir=/mnt/rocksdb` is a custom workload property for the `rocksdb` database interface layer to specify where `RocksDB` should write its data
- `-p recordcount=250000000` is a custom workload property to load 250M records into the database
- `-p operationcount=1000000000` is a custom workload property to run 1B operations against the database
- `-s` will periodically print out a status update to `stderr` so you can watch the progess of the workload
- `-threads 16` will run 16 threads in parallel reading and writing to `RocksDB`

Remember to run `blktrace` on the drive `RocksDB` is writing to. Repeat this for each of the file systems.

## Cassandra

`Cassandra` follows a server client architecture much like traditional databases. It writes the data to a data directory as well as a commit log to another directory. It also writes serveral other things like hints, saved_caches, etc., which are relatively small. Because of this, I've written all the data to one drive, and  the commit logs along with everything else to another drive.

### Setting up the Drives

For `Cassandra`, we will need 2 drives for the benchmarking, with an additional drive to store the `blktrace`. Set up two drives to the desired file system (the two drives will be using the same file system), and for the `blktrace`, set up an `ext4` file system.

For the purposes of this document, I will assume the drive for the data is `/dev/nvme0n1` mounted on `/mnt/data` and the drive for everything else is `/dev/nvme1n1` mounted on `/mnt/meta`.

Inside of `/mnt/meta` create 4 folders named `commitlogs`, `hints`, `logs`, and `saved_caches`. They will be used to store the meta data produced by `Cassandra`.

### Running Cassandra

Unlike `RocksDB`, you have to edit some of the configurations for `Cassandra`. Here we will configure the network settings as well as the data directories.

#### Configuring the Network

Assuming the machine is listening at `mel-14.syslab.sandbox`

Inside of `conf/cassandra.yaml`, we want to edit the following network configurations:

- set `.seed_provider[0].parameters[0].seeds` to `"mel-14.syslab.sandbox"`
- set `.listen_address` to `"mel-14.syslab.sandbox"`
- set `.rpc_address` to `"mel-14.syslab.sandbox"`

#### Configuring the Data Directories

Assuming you want the data directory to be at `/mnt/data`, and the meta data directories to be inside of `/mnt/meta` (this is the directory in which you created the 4 folders earlier),

Inside of `conf/cassandra-env.sh`, we want to edit the following directory configurations:

- set `JVM_OPTS` to include `/mnt/meta/logs/gc.log`

Inside of `conf/logback.xml`, we want to edit the following directory configurations:

- set `system.log` to be at `/mnt/meta/logs/system.log`
- set `debug.log` to be at `/mnt/meta/logs/debug.log`

Inside of `conf/cassandra.yaml`, we want to edit the following directory configurations:

- set `.hints_directory` to `/mnt/meta/hints`
- set `.data_file_directories` to `/mnt/data`
- set `.commitlog_directory` to `/mnt/meta/commitlog`
- set `.saved_caches_directory` to `/mnt/meta/saved_caches`

An example configuration can be found on `swift-012.syslab.sandbox` at `/home/xiaoche5/apache-cassandra-3.11.3.tar.xz`

#### Creating a Keyspace

Once you have the configurations set properly, simply run `./bin/cassandra` to start `Cassandra` as a daemon. Wait a few moments for `Cassandra` to finish starting up, then we will set up the keyspaces. Start `cqlsh` and connect to `Cassandra` as the address you set up earlier, and let's set up the keyspaces.

```
./bin/cqlsh mel-14.syslab.sandbox # start cqlsh to interact with Cassandra
cqlsh> create keyspace ycsb
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 3 };
cqlsh> USE ycsb;
cqlsh> create table usertable (
    y_id varchar primary key,
    field0 varchar,
    field1 varchar,
    field2 varchar,
    field3 varchar,
    field4 varchar,
    field5 varchar,
    field6 varchar,
    field7 varchar,
    field8 varchar,
    field9 varchar);
```

The replication factor of 3 above doesn't impact us in our case. This feature of `Cassandra` is meant to support high availability through data replication across multiple nodes and since we're only benchmarking on a single node, replication isn't important. Then we want to create a table within our keyspace that we can run our workload against.

### Running the Benchmark

```
./bin/ycsb load cassandra-cql -P workloads/workloada -p host=mel-14.syslab.sandbox -p recordcount=250000000 -p operationcount=1000000000 -s -threads 32
./bin/ycsb run cassandra-cql -P workloads/workloada -p host=mel-14.syslab.sandbox -p recordcount=250000000 -p operationcount=1000000000 -s -threads 32
```

Differences from the basic `YCSB` example from above:

- `cassandra-cql` is now the database interface layer rather than `basic`
- `-p hosts=mel-14.syslab.sandbox` is a custom workload property to run the workload against a `Cassandra` instance on a remote host
- `-p recordcount=250000000` is a custom workload property to load 250M records into the database
- `-p operationcount=1000000000` is a custom workload property to run 1B operations against the database
- `-s` will periodically print out a status update to `stderr` so you can watch the progess of the workload
- `-threads 32` will run 32 threads in parallel reading and writing to `RocksDB`

Remember to run `blktrace` on the drive `Cassandra` is writing to, that is both the data drive as well as the meta data drive. Repeat this for each of the file systems.

# Experiment

## blkparse

`blktrace` should have left you with several binary files containing the `blktrace` data, 1 per cpu core on the machine. We now want to generate a human readable file from this using `blkparse`. If the trace files are named `rocksdb_ext4_run_250M_1B.trace.blktrace.*`, we then want to use `blkparse` to generate the human readable trace as follows:

```
blkparse -a fs -i rocksdb_ext4_run_250M_1B.trace.blktrace -o rocksdb_ext4_run_250M_1B.trace
```

Repeat this for each set of `blktrace` data you have.

Note the `blktrace` data might be very large so this step can take quite some time. And pay attention to your disk space as the result can also be very large.

## Access Frequency

From the output of `blkparse`, we can then generate a csv mapping the page number to its access frequency. Using this python script, the output of `blkparse` can then be converted to the desired csv.

```
git clone https://github.com/Zylphrex/blkplt.git
cd blkplt/src
python main.py --infile blktrace.dump --outfile blktrace.csv --action A
```

You can run `python main.py --help` for more details.

## Saved Experiment Data

The results are not distributed online but you can find the results of `blktrace` on `mel-012.syslab.sandbox` at `/home/xiaoche5/traces`

The names of the files tells you the contents. They are named as follows:

`<database>_<filesystem>_<ycsb_action>_<recordcount>_<operationcount>.tar.xz`

So for example, `rocksdb_ext4_run_250M_1B.tar.xz` would contain the `blktrace` output from running `RocksDB` on an `ext4` file system during the `run` stage of `YCSB` with 250M records and 1B operations.

This folder also contain the results of running `blkparse -a fs -i <blktrace output>`. They are suffixed with `.trace.tar.xz`.

In addition, the access frequencies of these `blktraces` can be found on `swift-012.syslab.sandbox` at `/home/xiaoche5/frequencies.tar.xz`.

All the files in this folder are in a tar archive compressed using `xz` (specifically `pixz` for its parallel capabilities as well as indexing of tar archives). You can find the documentation for `pixz` here https://github.com/vasi/pixz.
