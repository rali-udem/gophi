$(document).ready(function() {
    var editor = ace.edit("amr");
    // editor.setShowPrintMargin(false);
    editor.setTheme("ace/theme/textmate");
    editor.getSession().setMode("ace/mode/amr");
    editor.setValue($('#inputAMR').text());
    $('#inputAMR').hide();
    editor.clearSelection();
    editor.renderer.updateFull(true); // try to force update but does not seem work
    var textarea = $('textarea[name="amr"]').hide();
    $("#form").submit(function(event) {
        textarea.val(editor.getSession().getValue());
    });
});
