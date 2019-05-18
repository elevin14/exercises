const palindromes = function(phrase) {
    phrase = phrase.replace(/[ .,!]/g,"");
    phrase = phrase.toLowerCase();
    for (let i = 0; i < phrase.length / 2; i++){
        if (phrase[i] != phrase[phrase.length - i - 1]){
            return false;
        }
    }
    return true;
}

module.exports = palindromes
