# Start the service and update with content
shinyServer(function(input, output,session) {

# This contains the updating of fields  
observe({   
metadata[cfg$elements,selected := is.element(cfg$elements, input$Checkbox)]
checkboxx <<-input$Checkbox

setsettings <<- data.table(Tr_size=as.numeric(input$Tr_size),
                         Tr_tgt=as.numeric(input$Tr_tgt)/100,
                         tst_size=as.numeric(input$tst_size),
                         rnd_seed=as.numeric(input$rnd_seed),
                         Target_Value=input$Target_Value, 
                         Target_Variable=input$Target_Variable)

# Store to cfg for export
cfg$Target_Variable <<-input$Target_Variable
cfg$Target_Value <<-input$Target_Value})

# Write some information to the GUI
observe(
  {output$dataset_details = renderText(paste0("File: ",filename,", ",
                                          "Variables: ",ncol(dataset),", ",
                                          "Total size: ",datalength,", ",
                                          "Sample size: ",nrow(dataset),", ",
                                          "Target %: ",100*nrow(dataset[get(input$Target_Variable)==input$Target_Value])/nrow(dataset)))
  })

# Set the cfg$elements to display
observe({
if(input$volgende_x[1] > next_button) {cfg$elements <<-cfg$elements+min(nrow(metadata)-max(cfg$elements),15); next_button<<-next_button+1}
if(input$vorige_x[1]   > last_button) {cfg$elements <<-cfg$elements-min(min(cfg$elements)-1,15); last_button<<-last_button+1}

# Update checkbox values
updateCheckboxGroupInput(session,"Checkbox", label = "", choices  = cfg$checkboxes[cfg$elements], 
                         selected = as.character(cfg$elements[1]-1+which(metadata[cfg$elements,selected]==1)));
})

# Write output if required
observe({
if(input$Gen_test_train[1] > createtrain) {Save_preprocess(setsettings,"Test_Train"); createtrain<<-createtrain+1};
if(input$Gen_full[1] > createfull) {Save_preprocess(setsettings,"Full"); createfull<<-createfull+1};
})

# Save the file if requested
observe({
if(input$save_to_file[1] > save_to_file)
{ metadata$selected = metadata$selected+0
  write.xlsx(metadata,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_metadata.xlsx"),sheetName="Sheet1",row.names=F);
  save_to_file <<- save_to_file + 1;
  isolate(cat("Saved settings to xlsx file \n"))}
})

observe({
updateSelectInput(session,"Target_Value", 
                  choices = 
                    setNames(laply(laply(unique((dataset[,input$Target_Variable,with=F])),as.character),as.list),
                             laply(laply(unique((dataset[,input$Target_Variable,with=F])),as.character),as.list)),
                  label = NULL, selected = NULL)
})


output$Chart_frequencies <- renderPlot(
frequencybar(dataset[,metadata$names[as.numeric(input$radiobutton[1])+cfg$elements[1]-1],with=F]),
)

output$Lift_graph <- renderPlot(
 Simple_Lift(dataset,input$Target_Variable,input$Target_Value,Variable_names[[as.numeric(input$radiobutton[1])+cfg$elements[1]-1]])
)

output$Cor_Table <- renderTable ({
cor_tabel = data.table(Variabele=Correlations$types$row.names,
                       Correlatie=Correlations$correlations[[as.numeric(input$radiobutton[1])+cfg$elements[1]-1]],
                       Methode=Correlations$types[[as.numeric(input$radiobutton[1])+cfg$elements[1]-1]])
cor_tabel = cor_tabel[pmatch(metadata$names,cor_tabel$Variabele)][metadata$selected]

setorder(cor_tabel,-Correlatie)
rbind(cor_tabel[Methode!="Error"][1:min(20,nrow(cor_tabel[Methode!="Error"]))],cor_tabel[Methode=="Error"])
})

})