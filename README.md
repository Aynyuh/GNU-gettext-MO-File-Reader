# GNU gettext MO File Reader
This class provides limited functions to read GNU MO binary files including plural forms. NOT IMPLEMENTED: contexts, domains, automatic translations files lookup

```delphi
var
  _: TGetTextProc;
  mo: TGNUMOFileReader;
begin
  mo := TGNUMOFileReader.Create('translations\de\messages.mo');

  try

    mo.Parse;
    _ := mo.Get_gettext;
    __ := mo.Get_ngettext;

    ShowMessageFmt(_('Welcome %s'), ['Luke']);
    // or
    // ShowMessageFmt(mo['Welcome %s'], ['Luke']);
    // or
    // ShowMessageFmt(mo.GetText('Welcome %s'), ['Luke']);

    ShowMessageFmt(__('You have %d new message', 'You have %d new messages', 2), [2]);
    // or
    // ShowMessageFmt(mo.Plurals['You have %d new message', 'You have %d new messages', 2], [2]);
    // or
    // ShowMessageFmt(mo.NGetText('You have %d new message', 'You have %d new messages', 2), [2]);

  finally
    mo.Free;
  end;
```

Sample PO file can be taken [here](https://localizely.com/po-file/). Use `msgfmt messages.po` to create binary mo file.
