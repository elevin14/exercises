// your javascript file
const container = document.querySelector('#container');

const content = document.createElement('div');
content.classList.add('content');
content.textContent = 'This is the glorious text-content!';

container.appendChild(content);

// Add a red paragraph
const para = document.createElement('p');
para.setAttribute('style','color: red;');
para.textContent = "Hey I'm red!";

container.appendChild(para);

// Add blue header
const header = document.createElement('h3');
header.setAttribute('style','color: blue;');
header.textContent = "I'm a blue h3!"

container.appendChild(header);

// Add div
const new_div = document.createElement('div');
new_div.setAttribute('style','border: 1px solid black; background: #ffb6c1')

const header_1 = document.createElement('h1');
header_1.textContent = "I'm in a div" 

const para_2 = document.createElement('p');
para_2.textContent = 'ME TOO!';

new_div.appendChild(header_1);
new_div.appendChild(para_2);

container.appendChild(new_div);