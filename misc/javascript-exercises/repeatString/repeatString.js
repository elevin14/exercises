const repeatString = function(str, n) {
    if (n < 0) {
        return 'ERROR'
    }
    return_str = ''
    for (let i = 0; i < n; i++){
        return_str += str
    }
    return return_str
}

module.exports = repeatString
