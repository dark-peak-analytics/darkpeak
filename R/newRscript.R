newRscript = function(filename=tempfile(tmpdir='.',fileext='.R'),
                open=TRUE) {

        lines = darkpeak::template_script

        lines = sub('Sys.time', format(Sys.time(), '%A, %d %B %Y'), lines)

        lines = sub('getwd', getwd(), lines)

        lines = sub('R.version.string', R.version.string, lines)

        writeLines(lines, filename)

        if (open) shell.exec(filename)

        filename

}


