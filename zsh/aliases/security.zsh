# wipe hard drive
security_wipe_hd () {
    # overwrite a drive with random data 7 times - for sensitive data
    for n in `seq 7`; do dd if=/dev/urandom of=$1 bs=8b conv=notrunc; done
}

make-password () {
    cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w $1 | head -n 1
}