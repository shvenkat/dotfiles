push () {
    array=("${array[@]}" "$1")
}

pop () {
    array=("${array[@]:0:$((${#array[@]}-1))}")
}

shift () {
    array=("${array[@]:1}")
}

unshift () {
    array=("$1" "${array[@]}")
}

function del_array_index {
    array=("${array[@]:0:$1}" "${array[@]:$(($1 + 1))}")
}

function del_array {
    local i
    for (( i = 0 ; i < ${#array[@]} ; i++ ))
    do
        if [ "$1" = "${array[$i]}" ]; then
            break
        fi
    done
    del_array_index $i
}

