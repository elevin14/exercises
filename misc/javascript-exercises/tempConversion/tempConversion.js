const ftoc = function(temp) {
  output = (temp-32)*5/9
  return format_output(output)
}

const ctof = function(temp) {
  output = temp*9/5+32
  return format_output(output)
}

function format_output(temp){
  if (temp % 1 != 0){
    return Number(temp.toFixed(1))
  }  
  return temp
}

module.exports = {
  ftoc,
  ctof
}
