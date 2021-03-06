library(rtf) 
rtf <- RTF("rtf_vignette.doc", width=8.5, height=11, font.size=10, 
           omi=c(1, 1, 1, 1)) 

addHeader(rtf, title="This text was added with the addHeader() function.",  #Titulo y subtitulo parte superior
          subtitle="So was this.") 
addParagraph(rtf, "This text was added with the addParagraph() function.  #Crea un p�rrafo de texto
             It is a new self-contained paragraph.  When &Alpha; is greater than 
             &beta;, then &gamma; is equal to zero.\n") 

startParagraph(rtf) #Deja un espacio y empieza un nuevo p�rrafo
addText(rtf, "This text was added with the startParagraph() and addText() 
        functions.  You can insert ") 
addText(rtf, "styled ", bold=TRUE, italic=TRUE)  #inserta texto con un estilo especiicado
addText(rtf, "text this way.  But, you must end the paragraph manually 
        with the endParagraph() function.\n") 
endParagraph(rtf) #Termina el parafo

increaseIndent(rtf) #Inicia otro parrafo con sangr�a
addParagraph(rtf, paste(rep("You can indent text with the increaseIndent() 
                            function.", 4), collapse="  ")) 

addNewLine(rtf) 

decreaseIndent(rtf) #Quita la sangr�a
addParagraph(rtf, paste(rep("And remove the indent with the 
decreaseIndent() function.", 4), collapse="  ")) 

addNewLine(rtf) 
addNewLine(rtf) 

addParagraph(rtf, "Table 1.  Table of the iris data using the addTable() 
function.\n") 
tab <- table(iris$Species, floor(iris$Sepal.Length)) 
names(dimnames(tab)) <- c("Species", "Sepal Length") 
addTable(rtf, tab, font.size=10, row.names=TRUE, NA.string="-", #Inserta la tabla creada anteriormente
         col.widths=c(1, 0.5, 0.5, 0.5, 0.5) ) 

newPlot <- function() { #Funci�n para crear un gr�fico
  par(pty="s", cex=0.7) 
  plot(iris[, 1], iris[, 2]) 
  abline(h=2.5, v=6.0, lty=2) 
} 

addPageBreak(rtf) #Inicia una nueva p�gina
addPlot(rtf, plot.fun=newPlot, width=5, height=5, res=300) #Inserta gr�fico
addNewLine(rtf) 
addParagraph(rtf, "Figure 1.  Plot of the iris data using the addPlot() 
function.\n") 

addNewLine(rtf) 
addNewLine(rtf) 

addSessionInfo(rtf) #Imprime la informaci�n referente a la sesi�n de trabajo
done(rtf)#Termina y escribe todo el documento