`CLNR` <-
function (Dir, project, note = "Files removed", test = TRUE) 
{
    FilesToRemove <- dir(path = Dir, full.names = TRUE, recursive = TRUE)
    FilesToRemoveComplete <- file.info(FilesToRemove)
    if (test == FALSE) {
        print(FilesToRemove)
        answer <- readline("Are you sure you want to delete these files? ")
        if (answer == "y" | answer == "Y") {
            file.remove(FilesToRemove)
            FilesToRemove <- c(note, FilesToRemove)
            FileNm <- glue(project, "/", "FilesRemoved_", 
                gsub("[[:space:]]|:", "_", date()), ".txt")
            write.table(FilesToRemoveComplete, file = FileNm, 
                sep = ",", quote = FALSE, row.names = TRUE, col.names = FALSE, 
                append = FALSE)
            answer2 <- readline(glue("Do you also want ", Dir, 
                " and all it's subdirectories removed? "))
            if (answer2 == "y" | answer2 == "Y") {
                setwd(project)
                if (win()) {
                  del.dir <- glue("cmd /C rmdir /S /Q ", "\"", 
                    Dir, "\"")
                  system(del.dir)
                }
                if (nix()){
                  deldir <- glue("rm -rf ", Dir)
                  system(deldir)
                }
            }
            else cat(glue("Files removed but ", Dir, " not removed", 
                "\n"))
        }
        else cat(glue("Removal of files aborted by user", "\n"))
    }
    else {
        cat(glue("Files that would have been deleted if test=TRUE", 
            "\n"))
        dir(path = Dir, full.names = TRUE, recursive = TRUE)
    }
}

