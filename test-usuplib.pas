program USupLibTest;


uses
    USupLib;


begin   
    WriteLn('Your new password is ', GeneratePassword());
    WriteLn('EncloseDoubleQuote(): ', EncloseDoubleQuote('testing double quote text'));
    WriteLn('EncloseSingleQuote(): ', EncloseSingleQuote('testing single quote text'));
    
end. // of program USupLibTest;