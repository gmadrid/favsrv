function Crawler(e, u) {
    this.entry = e
    e.progress = 0;
    this.url = u;
    this.index = 1;

    var img = new Image();
    this.img = img;
    img.onload = function() {
	this.loaded();
    }.bind(this);
    img.onerror = function() {
	this.errored();
    }.bind(this);
    img.src = u;
}

Crawler.prototype.loaded = function() {
    this.progress = 100;

    // do something
    this.entry.imageUrl = this.url.replace(/\d+\.media/, "" + (this.index - 1) + ".media");
};

Crawler.prototype.errored = function() {
    // check for overrun.

    this.progress = this.index;

    var newU = this.url.replace(/\d+\.media/, "" + this.index + ".media");
    this.index++;

    this.img.src = newU;
};










module.exports = Crawler;
