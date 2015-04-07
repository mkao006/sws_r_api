
saveMirroredData = function(requiredData){

    ## NOTE (Michael): We save back to complete trade flow as the
    ##                 mirror completes all the trade flow possibly
    ##                 observed.
    if(NROW(mirroredData) > 0)
        SaveData(domain = "trade", dataset = "completed_tf",
                 data = requiredData, normalized = FALSE)
}
