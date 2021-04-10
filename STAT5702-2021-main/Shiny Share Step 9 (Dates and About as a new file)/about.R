function(){
	tabPanel("About",
		HTML("<h1> Your name goes here</h1>
        <p >Say something sensible about the project you are doing.  Explain the project in a paragraph.   To use this as part of a job portfolio, include a picture of yourself.  I already have a job so I chose a picture of me working at an ice desk that I made when I visited University of Manitoba.  For job hunting purposes, you should use a picture that clearly identifies you.</p>
		<p>Make a new paragraph to say something about yourself. Use html code to make links to things like your LinkedIn, Twitter, Github, etc page.  Keep your code very basic to avoid breaking things.  Feel free to modify this template. Note that linking to social media isn't important for class, but it is very useful for building a portfolio of projects and skills. Although this paragraph can be very basic for our assignment, adding those elements in now will save you time when you are job hunting for a career or co-op job. </p>"
		),#end of html part 1.
        #Notice that I used double quotes (") above because otherwise it would interfere with
        # the single quote in the word (don't)
        HTML('
        <div style="clear: left;">
        <img src="https://people.math.carleton.ca/~davecampbell/Dave_Campbell_files/shapeimage_5.png" alt="" style="height: 274px; width: 204px; "> </div>
        <p>
        <a href="https://people.math.carleton.ca/~davecampbell/Dave_Campbell.html" target="_blank">Dr. Dave Campbell</a><br>
        Professor, School of Mathematics and Statistics<br>
        Carleton University<br>
        <a href="https://twitter.com/iamdavecampbell" target="_blank">Twitter</a><br>
        <a href="http://ca.linkedin.com/pub/dave-campbell/38/44/a84" target="_blank">Linkedin</a> <br/>
        </p>'),#End of html part 2
		value="about"
	)
}
