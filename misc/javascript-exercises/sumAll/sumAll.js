const sumAll = function(a, b) {
    if(typeof(a) != 'number' || typeof(b) != 'number'){
        return 'ERROR'
    }
    if (a<0 || b<0){
        return 'ERROR'
    }
    if (a>b){
        let c = a
        a = b
        b = c
    }
    return (a+b)*(b-a+1)/2
}

module.exports = sumAll
