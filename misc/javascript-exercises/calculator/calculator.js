function add (a,b) {
	return a+b;
}

function subtract (a,b) {
	return a-b;
}

function sum (a) {
	return a.reduce((num, currentValue) => (currentValue += num), 0)
}

function multiply (arr) {
	return arr.reduce((num, currentValue) => (currentValue *= num), 1)
}

function power(b,n) {
	return b ** n;
}

function factorial(n) {
	product = 1
	for (let i = 1; i <= n; i++){
		product *= i;
	}
	return product;
}

module.exports = {
	add,
	subtract,
	sum,
	multiply,
    power,
	factorial
}