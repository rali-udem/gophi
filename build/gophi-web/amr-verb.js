// indent an AMR according to parentheses level and starting a new line at each role
//   taking care of parentheses and colon within comments and strings 
function amrIndent(amrIn){
    var amrOut=""
    var inLines=amrIn.split("\n");
    var level=0
    for (var i = 0; i < inLines.length; i++) {
        var line=inLines[i].trim();
        amrOut+="    ".repeat(level);
        for (var j = 0; j < line.length; j++) {
            var c=line.charAt(j);
            if (c==";" || c=="#"){// comment, output rest of line
                amrOut+=line.substr(j);
                break;
            } else if (c=='"'){ // a string
                var strlen=line.substr(j+1).indexOf(c);
                if (strlen>=0){ // output string
                    amrOut+=line.substr(j,strlen+2);
                    j+=strlen+1
                } else { // no closing quote found, output rest of line
                    amrOut+=line.substr(j)
                    break;
                }
            } else {
                if (c==":" && j>0){ // start a new line at a role unless at the start
                    amrOut+="\n"+"    ".repeat(level);
                }
                amrOut+=c;
                if (c=='(') level++;
                else if (c==')') level--;
            } 
        }
        amrOut+="\n";
    }
    return amrOut.trim();
}

// taken from: https://ourcodeworld.com/articles/read/994/how-to-make-an-ace-editor-instance-resizable-by-the-user-dinamically-with-a-drag-and-drop-bar
/**
 * Global variable to store the ids of the status of the current dragged ace editor.
 */
window.draggingAceEditor = {};

function makeAceEditorResizable(editor){
    var id_editor = editor.container.id;
    var id_dragbar = '#' + id_editor + '_dragbar';
    var id_wrapper = '#' + id_editor + '_wrapper';
    var wpoffset = 0;
    window.draggingAceEditor[id_editor] = false;

    $(id_dragbar).mousedown(function(e) {
        // e.preventDefault();
        window.draggingAceEditor[id_editor] = true;
        var _editor = $('#' + id_editor);
        var top_offset = _editor.offset().top - wpoffset;
        // Set editor opacity to 0 to make transparent so our wrapper div shows
        _editor.css('opacity', 0);
        // handle mouse movement
        $(document).on('mousemove',function(e){
            var actualY = e.pageY - wpoffset;
            // editor height
            var eheight = actualY - top_offset;
            // Set wrapper height
            $(id_wrapper).css('height', eheight);
            // Set dragbar opacity while dragging (set to 0 to not show)
            $(id_dragbar).css('opacity', 0.15);
        });
    });
    
    $(document).mouseup(function(e){
        if (window.draggingAceEditor[id_editor]){
            var ctx_editor = $('#' + id_editor);
            var actualY = e.pageY - wpoffset;
            var top_offset = ctx_editor.offset().top - wpoffset;
            var eheight = actualY - top_offset;
            $( document ).off('mousemove');
            // Set dragbar opacity back to 1
            $(id_dragbar).css('opacity', 1);
            // Set height on actual editor element, and opacity back to 1
            ctx_editor.css('height', eheight-3).css('opacity', 1);
            // Trigger ace editor resize()
            editor.resize();
            window.draggingAceEditor[id_editor] = false;
            $("input[name='editorHeight']").prop("value",eheight-3);
        }
    });
}

$(document).ready(function() {
    var editor = ace.edit("amr");
    makeAceEditorResizable(editor)
    // editor.setShowPrintMargin(false);
    editor.setTheme("ace/theme/textmate");
    editor.getSession().setMode("ace/mode/amr");
    editor.setValue($('#inputAMR').text());
    $('#inputAMR').hide();
    editor.clearSelection();
    editor.renderer.updateFull(true); // try to force update but does not seem work
    var textarea$ = $('textarea[name="amr"]').hide();
    $("#form").submit(function(event) {
        textarea$.val(editor.getSession().getValue());
    });
    $("#indent").click(function(event){
        editor.setValue(amrIndent(editor.getSession().getValue()));
        editor.clearSelection();
    })
});
