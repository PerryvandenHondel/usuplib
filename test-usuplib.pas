program USupLibTest;


uses
    USupLib;


begin   
    WriteLn('Your new password is ', GeneratePassword());
    WriteLn('EncloseDoubleQuote(): ', EncloseDoubleQuote('testing double quote text'));
    WriteLn('EncloseSingleQuote(): ', EncloseSingleQuote('testing single quote text'));
    WriteLn('ReadSettingKey(): ', ReadSettingKey('test-usuplib.conf', 'Settings', 'TestSetting'));

    
end. // of program USupLibTest;