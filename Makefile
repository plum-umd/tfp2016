main:
	racket tfp2016.rkt

push:
	scp -r www/* lambdacalcul@tfp2016.org:tfp2016.org/
