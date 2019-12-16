filename='url.txt'
output='/home/jialun/microsoft_traces/'
n=1
while read line; do
# reading each line
echo "Line No. $n"
wget --directory-prefix=$output $line
n=$((n+1))
done < $filename
