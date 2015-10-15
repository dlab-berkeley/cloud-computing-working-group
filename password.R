#' Function to prepare a password based on some user defined options.
#'
#' @param length The password length, randomized by default. 
#' @param include The symbol categories to draw from. Numbers, lower and upper case letters, and symbols are all included by default.
#' @symbols A string specifying the symbols to include. By default they are drawn from the MSSND example.
#' @references https://security.berkeley.edu/minimum-security-standards-networked-devices-mssnd#five
#' @param lambda The mean of the poisson from which to define the probabilty distribution for draws.
#' @param display Whether to print the result to stdout.
#'
password<-function(
length=10 + rpois(1,10)
,include=c('numbers','letters','LETTERS','symbols')
,symbols='!@#$%^&*()_+|~-=\\`{}[]:\";\'<>?,./'
,lambda=3
,display=TRUE
){
	frame<-list()
	if('numbers'%in%include) frame$numbers<-0:9
	if('letters'%in%include) frame$letters<-letters
	if('LETTERS'%in%include) frame$LETTERS<-LETTERS
	if('symbols'%in%include) frame$symbols<-strsplit(symbols,split='')
	frame<-unlist(frame)
	pw<-sample(frame,length,replace=TRUE
		,prob=rpois(length(frame),lambda) # sample from an arbitrary probability distribution that makes it a little harder to brute force guess
	)
	pw<-paste(pw,collapse='')
	if(display) cat('Congrats, your password is:',pw,'\n')
	pw
}
password()
