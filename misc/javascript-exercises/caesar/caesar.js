const caesar = function (str, num) {
    num = (num % 26) + 26;
    strArr = str.split('')
    strArr = strArr.map((letter) => {
        let charCode = letter.charCodeAt(0);
        if (charCode >= 65 && charCode <= 90) {
            return String.fromCharCode((charCode + num - 65) % 26 + 65);
        } else if (charCode >= 97 && charCode <= 122) {
            return String.fromCharCode((charCode + num - 97) % 26 + 97);
        }
        return letter;
    });
    return strArr.join('');
}

module.exports = caesar
