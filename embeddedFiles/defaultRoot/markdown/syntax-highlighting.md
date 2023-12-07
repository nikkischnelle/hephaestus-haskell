```Java
void Il1egal0O(int null){
    switch(1|I[i]){ 
    case check_tab break;
    }
    return false;
}
```

```js
var undefined,
    xui,
    window     = this,
    string     = new String('string'),
    document   = window.document,
    simpleExpr = /^#?([\w-]+)$/,

```

```html
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html><head>
<title>A Tiny Page</title>
<style type="text/css">
<!--
      p { font-size:15pt; color:#000 }
    -->
</style></head><!-- real comment -->
<body bgcolor="#FFFFFF" text="#000000" link="#0000CC">
<script language="javascript" type="text/javascript">
      function changeHeight(h) {
        var tds = document.getElementsByTagName("td");
        for(var i = 0; i < tds.length; i++) {
          tds[i].setAttribute("height", h + "px");
      }}
</script>
<h1>abc</h1>
<h2>def</h2>
<p>Testing page</p>
</body></html>
```

```ruby
desc "Edit a post (defaults to most recent)"
task :edit_post, :title do |t, args|
  args.with_defaults(:title => false)
  posts = Dir.glob("#{source_dir}/#{posts_dir}/*.*")
  post = (args.title) ? post = posts.keep_if {|post| post =~ /#{args.title}/}.last : posts.last
  if post
    puts "Opening #{post} with #{editor}..."
    system "#{ENV['EDITOR']} #{post} &"
  else
    puts "No posts were found with \"#{args.title}\" in the title."
  end
end
```

```php
<?php
require_once($GLOBALS['g_campsiteDir']. "/$ADMIN_DIR/country/common.php");
require_once($GLOBALS['g_campsiteDir']. "/classes/SimplePager.php");
camp_load_translation_strings("api");
 
$f_country_language_selected = camp_session_get('f_language_selected', '');
$f_country_offset = camp_session_get('f_country_offset', 0);
if (empty($f_country_language_selected)) {
    $f_country_language_selected = null;
}
$ItemsPerPage = 20;
$languages = Language::GetLanguages(null, null, null, array(), array(), true);
$numCountries = Country::GetNumCountries($f_country_language_selected);
 
$pager = new SimplePager($numCountries, $ItemsPerPage, "index.php?");
 
$crumbs = array();
$crumbs[] = array(getGS("Configure"), "");
$crumbs[] = array(getGS("Countries"), "");
echo camp_html_breadcrumbs($crumbs);
 
?>
 
<?php  if ($g_user->hasPermission("ManageCountries")) { ?>
<table BORDER="0" CELLSPACING="0" CELLPADDING="1">
    <tr>
        <td><a href="add.php"><?php putGS("Add new"); ?></a></td>
    </tr>
</table>
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where
 
--import Prelude hiding (id)
--import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Monad (forM_)
-- import Data.Monoid (mempty, mconcat)
 
-- import System.FilePath
 
import Hakyll
 
 
main :: IO ()
main = hakyll $ do
 
    route   "css/*" $ setExtension "css"
    compile "css/*" $ byExtension (error "Not a (S)CSS file")
        [ (".css",  compressCssCompiler)
        , (".scss", sass)
        ]
 
    route   "js/**" idRoute
    compile "js/**" copyFileCompiler
 
    route   "img/*" idRoute
    compile "img/*" copyFileCompiler
 
    compile "templates/*" templateCompiler
 
    forM_ ["test.md", "index.md"] $ \page -> do
        route   page $ setExtension "html"
        compile page $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
 
sass :: Compiler Resource String
sass = getResourceString >>> unixFilter "sass" ["-s", "--scss"]
                         >>> arr compressCss
```

```bash
#!/bin/bash
 
cd $ROOT_DIR
DOT_FILES="lastpass weechat ssh Xauthority"
for dotfile in $DOT_FILES; do conform_link "$DATA_DIR/$dotfile" ".$dotfile"; done
 
# TODO: refactor with suffix variables (or common cron values)
 
case "$PLATFORM" in
    linux)
        #conform_link "$CONF_DIR/shell/zshenv" ".zshenv"
        crontab -l > $ROOT_DIR/tmp/crontab-conflict-arch
        cd $ROOT_DIR/$CONF_DIR/cron
        if [[ "$(diff ~/tmp/crontab-conflict-arch crontab-current-arch)" == ""
            ]];
            then # no difference with current backup
                logger "$LOG_PREFIX: crontab live settings match stored "\
                    "settings; no restore required"
                rm ~/tmp/crontab-conflict-arch
            else # current crontab settings in file do not match live settings
                crontab $ROOT_DIR/$CONF_DIR/cron/crontab-current-arch
                logger "$LOG_PREFIX: crontab stored settings conflict with "\
                    "live settings; stored settings restored. "\
                    "Previous settings recorded in ~/tmp/crontab-conflict-arch."
        fi
    ;;
```

```python
# test python (sample from offlineimap)
 
class ExitNotifyThread(Thread):
    """This class is designed to alert a "monitor" to the fact that a thread has
    exited and to provide for the ability for it to find out why."""
    def run(self):
        global exitthreads, profiledir
        self.threadid = thread.get_ident()
        try:
            if not profiledir:          # normal case
                Thread.run(self)
            else:
                try:
                    import cProfile as profile
                except ImportError:
                    import profile
                prof = profile.Profile()
                try:
                    prof = prof.runctx("Thread.run(self)", globals(), locals())
                except SystemExit:
                    pass
                prof.dump_stats( \
                            profiledir + "/" + str(self.threadid) + "_" + \
                            self.getName() + ".prof")
        except:
            self.setExitCause('EXCEPTION')
            if sys:
                self.setExitException(sys.exc_info()[1])
                tb = traceback.format_exc()
                self.setExitStackTrace(tb)
        else:
            self.setExitCause('NORMAL')
        if not hasattr(self, 'exitmessage'):
            self.setExitMessage(None)
 
        if exitthreads:
            exitthreads.put(self, True)
 
    def setExitCause(self, cause):
        self.exitcause = cause
    def getExitCause(self):
        """Returns the cause of the exit, one of:
        'EXCEPTION' -- the thread aborted because of an exception
        'NORMAL' -- normal termination."""
        return self.exitcause
    def setExitException(self, exc):
        self.exitexception = exc
    def getExitException(self):
        """If getExitCause() is 'EXCEPTION', holds the value from
        sys.exc_info()[1] for this exception."""
        return self.exitexception
    def setExitStackTrace(self, st):
        self.exitstacktrace = st
    def getExitStackTrace(self):
        """If getExitCause() is 'EXCEPTION', returns a string representing
        the stack trace for this exception."""
        return self.exitstacktrace
    def setExitMessage(self, msg):
        """Sets the exit message to be fetched by a subsequent call to
        getExitMessage.  This message may be any object or type except
        None."""
        self.exitmessage = msg
    def getExitMessage(self):
        """For any exit cause, returns the message previously set by
        a call to setExitMessage(), or None if there was no such message
        set."""
        return self.exitmessage
```

```c
#define UNICODE
#include <windows.h>
 
int main(int argc, char **argv) {
  int speed = 0, speed1 = 0, speed2 = 0; // 1-20
  printf("Set Mouse Speed by Maverick\n");
 
  SystemParametersInfo(SPI_GETMOUSESPEED, 0, &speed, 0);
  printf("Current speed: %2d\n", speed);
 
  if (argc == 1) return 0;
  if (argc >= 2) sscanf(argv[1], "%d", &speed1);
  if (argc >= 3) sscanf(argv[2], "%d", &speed2);
 
  if (argc == 2) // set speed to first value
    speed = speed1;
  else if (speed == speed1 || speed == speed2) // alternate
    speed = speed1 + speed2 - speed;
  else
    speed = speed1;  // start with first value
 
  SystemParametersInfo(SPI_SETMOUSESPEED, 0,  speed, 0);
  SystemParametersInfo(SPI_GETMOUSESPEED, 0, &speed, 0);
  printf("New speed:     %2d\n", speed);
  return 0;
}
```

```rust
pub struct ClassedHTMLGenerator<'a> {
    syntax_set: &'a SyntaxSet,
    open_spans: isize,
    parse_state: ParseState,
    scope_stack: ScopeStack,
    html: String,
    style: ClassStyle,
}

impl<'a> ClassedHTMLGenerator<'a> {
    #[deprecated(since="4.2.0", note="Please use `new_with_class_style` instead")]
    pub fn new(syntax_reference: &'a SyntaxReference, syntax_set: &'a SyntaxSet) -> ClassedHTMLGenerator<'a> {
        Self::new_with_class_style(syntax_reference, syntax_set, ClassStyle::Spaced)
    }

    pub fn new_with_class_style(
        syntax_reference: &'a SyntaxReference,
        syntax_set: &'a SyntaxSet,
        style: ClassStyle,
    ) -> ClassedHTMLGenerator<'a> {
        let parse_state = ParseState::new(syntax_reference);
        let open_spans = 0;
        let html = String::new();
        let scope_stack = ScopeStack::new();
        ClassedHTMLGenerator {
            syntax_set,
            open_spans,
            parse_state,
            scope_stack,
            html,
            style,
        }
    }

    /// Parse the line of code and update the internal HTML buffer with tagged HTML
    ///
    /// *Note:* This function requires `line` to include a newline at the end and
    /// also use of the `load_defaults_newlines` version of the syntaxes.
    pub fn parse_html_for_line_which_includes_newline(&mut self, line: &str) -> Result<(), Error>{
        let parsed_line = self.parse_state.parse_line(line, self.syntax_set)?;
        let (formatted_line, delta) = line_tokens_to_classed_spans(
            line,
            parsed_line.as_slice(),
            self.style,
            &mut self.scope_stack,
        )?;
        self.open_spans += delta;
        self.html.push_str(formatted_line.as_str());

        Ok(())
    }

    /// Parse the line of code and update the internal HTML buffer with tagged HTML
    ///
    /// ## Warning
    /// Due to an unfortunate oversight this function adds a newline after the HTML line,
    /// and thus requires lines to be passed without newlines in them, and thus requires
    /// usage of the `load_defaults_nonewlines` version of the default syntaxes.
    ///
    /// These versions of the syntaxes can have occasionally incorrect highlighting
    /// but this function can't be changed without breaking compatibility so is deprecated.
    #[deprecated(since="4.5.0", note="Please use `parse_html_for_line_which_includes_newline` instead")]
    pub fn parse_html_for_line(&mut self, line: &str) {
        self.parse_html_for_line_which_includes_newline(line).expect("Please use `parse_html_for_line_which_includes_newline` instead");
        // retain newline
        self.html.push('\n');
    }

    /// Close all open `<span>` tags and return the finished HTML string
    pub fn finalize(mut self) -> String {
        for _ in 0..self.open_spans {
            self.html.push_str("</span>");
        }
        self.html
    }
}
```