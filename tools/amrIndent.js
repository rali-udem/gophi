function amrIndent(amrIn){
    var amrOut=""
    var inLines=amrIn.split("\n");
    var level=0
    for (var i = 0; i < inLines.length; i++) {
        var line=inLines[i].trim();
        if (line.startsWith("#")){
            amrOut+=inLines[i]+"\n";
        } else {
            var line1=line.split(/(?=:)/g)
            for (var j = 0; j < line1.length; j++) {
                var line2=line1[j];
                amrOut+="    ".repeat(level)+line2+"\n"
                for (var k = 0; k < line2.length; k++) {
                    var c=line2[k];
                    if (c=="(")level++;
                    else if (c==")")level--;
                }
            }
        }
    }
    return amrOut;
}
