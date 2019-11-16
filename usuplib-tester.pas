program USupLibTest;


uses
    USupLib;


begin   
    //WriteLn('Your new password is ', GeneratePassword());
    //WriteLn('EncloseDoubleQuote(): ', EncloseDoubleQuote('testing double quote text'));
    //WriteLn('EncloseSingleQuote(): ', EncloseSingleQuote('testing single quote text'));
    //WriteLn('ReadSettingKey(): ', ReadSettingKey('test-usuplib.conf', 'Settings', 'TestSetting'));

    WriteLn(Occurs('text1;text2;text3;text4', ';'));
    
end. // of program USupLibTest;