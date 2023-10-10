chapter1.html : chapter1.md
	pandoc -o chapter1.html --css=styling.css -s --metadata title="Chapter 1 - Basics" chapter1.md
