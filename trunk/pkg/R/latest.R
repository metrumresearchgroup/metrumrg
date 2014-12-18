`latest` <-
function(x)as.character(
  tapply(
    x,
    INDEX=dirname(dirname(x)),
    FUN=function(group)rev(sort(group))[[1]]
  )
)

