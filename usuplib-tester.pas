program USupLibTest;


uses
    SysUtils,
    USupLib;


var
    a: TStringArray;   // declare the array a
    x: integer;

begin   
    //WriteLn('Your new password is ', GeneratePassword());
    //WriteLn('EncloseDoubleQuote(): ', EncloseDoubleQuote('testing double quote text'));
    //WriteLn('EncloseSingleQuote(): ', EncloseSingleQuote('testing single quote text'));
    //WriteLn('ReadSettingKey(): ', ReadSettingKey('test-usuplib.conf', 'Settings', 'TestSetting'));

    WriteLn(Occurs('text1;text2;text3;text4', ';'));

    SetLength(a, 0);    // initialize array a
    a := SplitString('part1;part2;part3;partofom4;p5;partie6;paty7', ';');
    for x := 0 to High(a) do 
    begin
        // For every part of the array print it
        WriteLn(IntToStr(x) + ': ' + a[x]);
    end;
    SetLength(a, 0)     // Clear the memory used
end. // of program USupLibTest;