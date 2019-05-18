const removeFromArray = function(arr) {
    let values = Array.from(arguments)
    values.shift()
    for (let i = arr.length - 1; i >= 0; i--){
        if (values.includes(arr[i])){
            arr.splice(i,1);
        }
    }
    return arr;
}

module.exports = removeFromArray
