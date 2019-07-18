function Book(title,author,pages,read) {
    this.title = title
    this.author = author
    this.pages = pages
    this.read = read
    this.info = function() {
        let readYet = read ? 'read' : 'not read yet'
        return title + ' by ' + author + ', ' + pages + ' pages, ' + readYet
    }
}

theHobbit = new Book('The Hobbit','J.R.R. Tolkien',295,false)
console.log(theHobbit.info());