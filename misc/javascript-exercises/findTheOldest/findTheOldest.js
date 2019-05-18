let findTheOldest = function(arr) {
    return arr.reduce((oldest,currentValue) =>{
        oldestAge = ('yearOfDeath' in oldest ? oldest.yearOfDeath : new Date().getFullYear()) - oldest.yearOfBirth;
        currentAge = ('yearOfDeath' in currentValue ? currentValue.yearOfDeath : new Date().getFullYear()) - currentValue.yearOfBirth;
        return oldestAge > currentAge ? oldest : currentValue;
    });
}

module.exports = findTheOldest
