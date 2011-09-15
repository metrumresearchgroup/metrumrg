.onLoad <-function(lib,pkg)
{
    methods::setClass(
    	"nm.data", 
    	methods::representation(
    		data = "data.frame", 
    		eta = "data.frame", 
    		theta = "data.frame", 
    		omega = "data.frame", 
    		sigma = "data.frame"
    	)
    )
    cat('metrumrg',utils::packageDescription('metrumrg',fields='Version'),'\n')
}
