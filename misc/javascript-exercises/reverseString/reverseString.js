const reverseString = function(str) {
    str_reverse = '';
    for (let i = str.length - 1; i >= 0; i--){
        str_reverse += str[i];
    }
    return str_reverse;
}

module.exports = reverseString
