//	---------------------------------------------------------------------------
//	---------------------------------------------------------------------------
//
//	NAME:
//		Support Library
//
//	DESCRIPTION:
//		General unit with functions and procedures to be used in Pascal programs.
//
//		Copyright (c) 2014
//
//	HISTORY:
//		2015-06-04	Added function GetLocalIp()
//		2015-06-05 	Added function GetDnsDomain()
//		2016-11-30	Added function GetSecondsFromNow
//
//	---------------------------------------------------------------------------



unit USupportLibrary;



{$mode objfpc}
{$H+}



interface



uses
	SysUtils,
	Windows,
	Classes,
	Process,
	DateUtils,
	DOS,
	StrUtils,
	Winsock;	// for function GetLocalIp()


	
type
	TStringArray = array of string;

	COMPUTER_NAME_FORMAT = (
		ComputerNameNetBIOS,
		ComputerNameDnsHostname,
		ComputerNameDnsDomain,
		ComputerNameDnsFullyQualified,
		ComputerNamePhysicalNetBIOS,
		ComputerNamePhysicalDnsHostname,
		ComputerNamePhysicalDnsDomain,
		ComputerNamePhysicalDnsFullyQualified,
		ComputerNameMax);

	

const	
   // InstrEx mode flags
   isForward  = 		0;
   isBackward = 		1;
   isNumber = 			2;
   isNoCase = 			4;
   
   WriteModStep = 		567;
   
   TAB = 				#9;
   
   
   
// Text en strings functions 
function AlignLeft(n: integer; l: integer): string;						// Returns a string left aligned on l length - Number. > [83764        ]
function AlignLeft(s: string; l: integer): string;						// Returns a string left aligned on l length - String. > [text         ]
function AlignRight(n: integer; l: integer): string;					// Returns a string right aligned on l length - Number. > [        8343]
function AlignRight(s: string; l: integer): string;						// Returns a string right aligned on l length - String. > [        text]
function BoolStrToBool(setting: AnsiString): boolean;					// Convert a string to a boolean
function EncloseDoubleQuote(const s: string): string;					// Enclose the string s with double quotes: s > "s".
function EncloseSingleQuote(const s: string): string;					// Enclose the string s with single qoutes: s > 's'.
function LastCharPos(const S: string; const Chr: char): integer;
function GeneratePassword(): string;									// Generate a new password in format XXXXxxxx9999!!
function GetRandomString(l: integer): string;							// Get a string of l length with random chars.


// Date Time functions
function ConvertDateTimeIso8601ToSystem(dt: string): TDateTime;
function DateDiffSec(Date1, Date2: TDateTime): int64;					// Returns the number of seconds between 2 date times.
function ConvertDateTimeToIso8601(dt: TDateTime): Ansistring;			// Return the date in format YYYY-MM-DD HH:MM:SS
function GetDateFs(const bUseSeparator: boolean): string;
function GetMostRecent(dt1: TDateTime; dt2: TDateTime): TDateTime;		// Returns the most recent date of 2 date time variables.
function GetProperDateTime(dt: TDateTime): string;
function GetTimeFs(): string;


// Folder and files functions
function FixFolderAdd(const f: string): string;							// Add a trailing backslash at the end of the folder name.
function FixFolderRemove(const f: string): string;						// Remove the trailing '\' from a folder. d:\folder\ >> d:\folder.
function GetPathOfPidFile(directoryToStoreThePidFile: Ansistring): Ansistring; // Returns the path of a pid (Process ID) file.
function GetProgramFolder(): string;									// Returns the folder to the current program.
function GetProgramName(): string;										// Returns the program name of the current program.
function GetProgramPath(): string;										// Full path to the current program.


// Computer, User en AD functions
function GetBaseDn(): string;											// Get the BaseDN of the current AD domain, use adfind.exe
function GetComputerNameExString(ANameFormat: COMPUTER_NAME_FORMAT): WideString;
function GetCurrentComputerName: string;
function GetCurrentUserName: string;
function GetCurrentPid: integer;										// Return the current Process ID (PID)
function GetDnsDomain(): string;										// Returns the DNS domain name of this user.
function GetDrive(p: string): string;
function GetFileSizeInBytes(sPath: string): integer;
function GetLocalIp(): string;											// Returns the local IP of the computer.
function GetNetbiosDomain(): string;									// Returns the NetBIOS domain name.
function IsAccountLockedOut(dn: string): boolean;						// Check if an account is locked in the AD using the DN



function HexToInt(hstr:string): integer;
function LineCount(fname: string): integer;
function NumberAlign(v: integer; l: byte): string;
function Occurs(str: AnsiString; separator: string): integer;
function ConvertPathTemplateToRealPath(inString: AnsiString): Ansistring;
function ReadSettingKey(path: AnsiString; section: AnsiString; key: AnsiString): AnsiString;
function ResolveFqdn(ip: string): string;
function RollingLogFile(const p: string; maxLogs: integer): string;
function RunCommand(strCommand: Ansistring): integer;
function SplitString(str: AnsiString; sep: string): TStringArray; 		// V02 str: string > AnsiString
procedure MakeFolderTree(p : string);									// Make a folder tree when it doesn't exist
procedure WaitSecs(s: integer);
procedure WriteMod(count: LongInt; step: LongInt; txt: string);			// Write a line every step.
procedure LifeCheck();
function GetSecondsFromNow(checkYear: word; checkMonth: word; checkDay: word): longint;


implementation



uses
	UTextFile;
	
	
var
	globalPid: integer;				// The PID of the process.


// ================================================================================================
// FUNCTIONS
// ================================================================================================


function GetComputerNameExW(NameType: COMPUTER_NAME_FORMAT; lpBuffer: LPWSTR;
  var nSize: DWORD): BOOL; stdcall; external kernel32 name 'GetComputerNameExW';
  // Used by function GetComputerNameExString


  
function BoolStrToBool(setting: AnsiString): boolean;
//
//	Convert a boolean string to a boolean.
//	
//	Example:
//		WriteLn('return=', BoolStrToBool('YES')); will print 'return=TRUE' 
//
var
	r: boolean;
begin
	setting := UpperCase(setting);
	
	case setting of
		'YES', 'TRUE',  '1', 'WAAR': r := true;
		'NO',  'FALSE', '0', 'ONWAAR': r := false;
	else
		WriteLn('BoolStrToBool(): WARNING cannot convert setting ', setting, ' to a boolean value!');
		r := false;
	end;
	BoolStrToBool := r;
end; // of function BoolStrToBool



function LastCharPos(const S: string; const Chr: char): integer;
//
//	Find the last occurrence of char in a string
//
//	Source: http://stackoverflow.com/questions/5844406/find-the-last-occurrence-of-char-in-a-string
//
var
  i: Integer;
begin
  result := 0;
  for i := length(S) downto 1 do
    if S[i] = Chr then
      Exit(i);
end;
  


function GetComputerNameExString(ANameFormat: COMPUTER_NAME_FORMAT): WideString;
//
// Source: http://stackoverflow.com/questions/8446940/how-to-get-fully-qualified-domain-name-on-windows-in-delphi
//
//                  ComputerNameNetBIOS=NSH5DT00037
//              ComputerNameDnsHostname=NSH5DT00037
//                ComputerNameDnsDomain=prod.ns.nl
//        ComputerNameDnsFullyQualified=NSH5DT00037.prod.ns.nl
//          ComputerNamePhysicalNetBIOS=NSH5DT00037
//      ComputerNamePhysicalDnsHostname=NSH5DT00037
//        ComputerNamePhysicalDnsDomain=prod.ns.nl
//ComputerNamePhysicalDnsFullyQualified=NSH5DT00037.prod.ns.nl
//                      ComputerNameMax=
//
var
  nSize: DWORD;
begin
  nSize := 1024;
  SetLength(Result, nSize);
  if GetComputerNameExW(ANameFormat, PWideChar(Result), nSize) then
    SetLength(Result, nSize)
  else
    Result := '';
end; // of function GetComputerNameExString



function GetCurrentPid: integer;
begin
	if globalPid = 0 then
	begin
		WriteLn('WARNING GetCurrentPid(): Use the procedure GetPathOfPidFile() before GetCurrentPid() in the code.');
		GetCurrentPid := 0;
	end
	else
		GetCurrentPid := globalPid;
end; // of GetCurrentPid()



function IsAccountLockedOut(dn: string): boolean;
//
//	Check if an account is locked
//
//		true:		Account is locked in the AD
//		false:		Account is not locked in the AD
//
var
	path: string;
	p: TProcess;
	f: TextFile;
	line: string;	// Read a line from the nslookup.tmp file.
	//r: boolean;		// Result of the function to return.
	lt: string;
begin
	//r := false;
	lt := '';

	// Get a temp file to store the output of the adfind.exe command.
	path := SysUtils.GetTempFileName(); // Path is C:\Users\<username>\AppData\Local\Temp\TMP00000.tmp
	WriteLn(path);
	
	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c adfind.exe -b "' + dn + '" lockoutTime >' + path);
	p.Options := [poWaitOnExit];
	p.Execute;
	
	// Open the text file and read the lines from it.
	Assign(f, path);
	
	{I+}
	Reset(f);
	repeat
		ReadLn(f, line);
		if Pos('>lockoutTime: ', line) > 0 then
		begin
			// This is the line with 'Name:' in it.
			// Remove 'Name:' from the line and trim the result.
			lt := Trim(StringReplace(line, '>lockoutTime: ', '', [rfIgnoreCase])); // Only get the last part after '>lockoutTime: ' 
		end;
	until Eof(f);
	Close(f);
	
	SysUtils.DeleteFile(path);
	
	if lt = '0' then
		IsAccountLockedOut := false		// Account is not locked out, lockoutTime=0
	else
		IsAccountLockedOut := true;		// Account is locked out, lockoutTime=XXXXXXXXXXXXXXX
end; // of function IsAccountLockedOut



function RunCommand(strCommand: Ansistring): integer;
//
//	Run a command using the CMD.EXE interpreter, 
//	returns the error level as the result of this function.
//
var
	p: TProcess;		// Uses Process
begin
	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c ' + strCommand);
	// Options:
	//	poWaitOnExit: //
	//	poNoConsole:
	//	poUsePipes: 
	p.Options := [poWaitOnExit, poUsePipes];
	//p.Options := [poWaitOnExit, poNoConsole];
	p.Execute;
	
	Result := p.ExitStatus;
	p.Free;
end; // of function RunCommand.



function GeneratePassword(): string;
//
//	Generate a new password using chars in S.
//	
//	Format: XXXXxxxx9999!!
//
const 
	lowerChar = 'abcdefghijklmnopqrstuvwxyz';
	upperChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
	numberChar = '0123456789';
	specialChar = '!@#$%'; 
var 
	CharCount: integer; 
begin result := '';
	// Initialize the random number generator.
	Randomize;
	
	// Get 4 upper chars.
	for CharCount := 1 to 4 do 
		result := result + upperChar[random(length(upperChar)) + 1];
		
	result := result + '-';
	// Get 4 lower chars.
	for CharCount := 1 to 4 do 
		result := result + lowerChar[random(length(lowerChar)) + 1];

	result := result + '-';	

	// Get 4 number chars.
	for CharCount := 1 to 4 do 
		result := result + numberChar[random(length(numberChar)) + 1];
end; // of function GeneratePassword.



function DateDiffSec(Date1, Date2: TDateTime): int64;
//
//	Calculate the number of seconds between 2 dates.
//	
//	Result:
//		< 0		Date2 is newer then Date1
//		> 0		Date1 is newer then Date2
//		= 0		Date1 is the same as Date2
//
var
	r: integer;
begin
	r := Trunc(86400.0 * (Date1 - Date2)); // Number of seconds between 2 dates
	//WriteLn('DateDiffSec(): Date1=', DateTimeToStr(Date1), ' - Date2=', DateTimeToStr(Date2), ' = ', r);
	DateDiffSec := r;
end; // of function DateDiffSec().



function GetSecondsFromNow(checkYear: word; checkMonth: word; checkDay: word): longint;
//
//	Get the numbers of seconds between 2 dates.
//	
//	GetSecondsFromNow(2016, 12, 1)
//
//	When return < Now then the date is in the future.
//	When return > Now then the date is in the past.
//	
//	Uses:
//		DateDiffSec (USupportLibrary)
//
var
	n: TDateTime;
	c: TDateTime;
	
begin
	n := Now();
	c := EncodeDate(checkYear, checkMonth, checkDay);
	
	//WriteLn('GetSecondsFromNow():');
	//WriteLn('  n=', DateToStr(n));
	//WriteLn('  c=', DateToStr(c));
	GetSecondsFromNow := DateDiffSec(n, c);
end; // of function GetSecondsFromNow



function GetMostRecent(dt1: TDateTime; dt2: TDateTime): TDateTime;
//
//	Returns the newest date time based on 2 date times.
//	
//	Usage:
//		r := DateTimeToStr(GetMostRecent(StrToDateTime('2015-07-01 09:00:00'), StrToDateTime('2015-07-01 09:00:02')));
//		WriteLn(r); shows 2015-07-01 09:00:02
//	
var
	r: longint;
begin
	r := DateDiffSec(dt1, dt2);
	//WriteLn('GetMostRecent:');
	//WriteLn('  dt1: ', DateTimeToStr(dt1));
	//WriteLn('  dt2: ', DateTimeToStr(dt2));
	//WriteLn('    r: ', r);
	if r > 0 then
	begin
		//WriteLn('Most Resent is dt1: ', DateTimeToStr(dt1));
		GetMostRecent := dt1;
	end
	else
	begin
		//WriteLn('Most Resent is dt2: ', DateTimeToStr(dt2));
		GetMostRecent := dt2;
	end;
end; // of function GetMostRecent



function GetRandomString(l: integer): string;
var
	i: integer;
	sValidChars: string;
	r: string;				// Return value
begin
	// List of valid chars. Pick one at a time.
	sValidChars := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz';
	//sValidChars := '0123456789abcdef';
	
	// When missing this sleep command, the chance exists to get the same code back.
	// If the GetRandomString function is requested directly after each other.
	Sleep(1);
	
	// Initialize the random number generator.
	Randomize;
	
	r := '';
	for i := 1 to l do
	begin
		//WriteLn(i, TAB, sValidChars[Random(Length(sValidChars))+1]);
		r := r + sValidChars[Random(Length(sValidChars))+1]
	end; // of for
	GetRandomString := r;
end; // of function GetRandomString().



function GetFileSizeInBytes(sPath: string): integer;
//
// Returns the size of the file sPath in bytes
// Maximum supports 2G files.
//
// Source: https://helloacm.com/how-to-get-file-size-in-bytes-using-delphi-object-pascal/
//
var
  f: File of byte;
begin
  Result := -1;
  if (FileExists(sPath)) then
  begin
    try
      {$I-}
      AssignFile(f, sPath);
      Reset(f);
      {$I+}
      if (IOResult = 0) then
      begin
        Result := FileSize(f);
      end
      else
      begin
        Result := 0;
      end;
    finally
      {$I-}CloseFile(f);{$I+}
    end;
  end;
end; // of function GetFileSizeInBytes.



function GetPathOfPidFile(directoryToStoreThePidFile: Ansistring): Ansistring;
//
//	Create a Unix-style PID file. The file contains the current Process ID.
//	resultPath will be the current path of the executable but with an .pid extension.
//
//	Delete this file at the end of the program. When found at start of the program.
//	the program is already running. Can be a check for multiple startings of
//	this program.
//
//	Example of use:
//		pathPid := GetPathOfPidFile(GetProgramFolder());
//
var
	resultPath: string;
	processId: integer;
	f: TextFile;
	programName: Ansistring;
begin
	processId := GetProcessID();
	programName := GetProgramName(); // Contaions the current .EXE name of the program.
	resultPath := FixFolderAdd(directoryToStoreThePidFile) + programName + '.pid' + IntToStr(processId);
	//WriteLn('GetPathOfPidFile(): resultPath=', resultPath);
	
	globalPid := processId;
	
	AssignFile(f, resultPath);
	{I+}
	ReWrite(f);
	WriteLn(f, programName + ' is running under process id: ' + IntToStr(processId));
	CloseFile(f);
	Result := resultPath;
end; // of function GetPathOfPidFile.



function GetLocalIp(): string;
//
// Returns the local IP address.
//
// Return:
//	192.168.0.10
//
//	Uses Winsock dll for resolving the IP address.
//
// Source: http://free-pascal-general.1045716.n5.nabble.com/lNet-getting-the-local-IP-td3200339.html
//
//
const
	CFormatIPMask = '%d.%d.%d.%d'; 
var
	VName: string;
	VHostEnt: PHostEnt;
	VWSAData: TWSAData; 
begin
	Result := '';
	// The WSAStartUp function initializes the WinSock2 library for use by your program, 
	// and must be the first WinSock2 library function called by your program.
	// Source: http://www.irietools.com/iriepascal/progref374.html
	WSAStartup(2, VWSAData); 
	
	SetLength(VName, 255); 
    GetHostName(PChar(VName), 255); 
    SetLength(VName, StrLen(PChar(VName))); 
    VHostEnt := GetHostByName(PChar(VName)); 
    with VHostEnt^ do 
		Result := Format(CFormatIPMask, [Byte(h_addr^[0]), Byte(h_addr^[1]), Byte(h_addr^[2]), Byte(h_addr^[3])]); 
	WSACleanup;
end; // of function GetLocalIp()



function ResolveFqdn(ip: string): string;
//
// Resolve a Fully Qualified Domain Name (FQDN) from a IP address.
// Using nslookup.exe.
//		
//		C:\>nslookup 10.4.68.19
//		Server:  ns00nc0102.ns.nl
//		Address:  10.4.34.11
//
//		Name:    ns00dc014.prod.ns.nl
//		Address:  10.4.68.19
//
const
	TMP_FILE = 'nslookup.tmp';
var	
	p: TProcess;
	f: TextFile;
	line: string;	// Read a line from the nslookup.tmp file.
	r: string;		// Result of the function to return.
begin
	// Result will be the ip address when no resolve can be done.
	r := ip;
	
	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c nslookup -timeout=5 ' + ip + '>' + TMP_FILE);
	p.Options := [poWaitOnExit];
	p.Execute;
	
	// Open the text file and read the lines from it.
	Assign(f, TMP_FILE);
	
	{I+}
	Reset(f);
	repeat
		ReadLn(f, line);
		//WriteLn('ResolveFqdn(): ', line);
		if Pos('Name:', line) > 0 then
		begin
			// This is the line with 'Name:' in it.
			// Remove 'Name:' from the line and trim the result.
			r := Trim(StringReplace(line, 'Name:', '', [rfIgnoreCase]));
		end;
	until Eof(f);
	Close(f);
	
	ResolveFqdn := r;
end; // of function ResolveFqdn.



function GetBaseDn(): string;
//
// Get the BaseDN (DC=domain,DC=ext) of the current AD domain.
// Use adfind.exe to obtain the information.
//
const
	FILENAME = 'basedn.tmp';
var
	p: TProcess;
	f: TextFile;
	line: string;
	r: string;
begin
	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
	// Doe not ready matter what we look for.
	// We need the 3 line of the standard output of adfind.exe
    p.Parameters.Add('/c adfind.exe -f "Whatever=*" >' + FILENAME);
	p.Options := [poWaitOnExit];

	p.Execute;
	
	Assign(f, FILENAME);
	
	{I+}
	Reset(f);
	while not eof(f) do
	begin
		ReadLn(f, line);
		if Pos('Base DN: ', line) > 0 then
		begin
			r := Trim(StringReplace(line,  'Base DN: ', '', [rfIgnoreCase]));
			// No need to go further because we found it, break this while loop.
			break;
		end;
	end;
	Close(f);
	
	DeleteFile(FILENAME);
	
	GetBaseDn := r;
end; // of function GetBaseDn



function ConvertDateTimeIso8601ToSystem(dt: string): TDateTime;
//
//	Convert a Date Time string in format YYYY-MM-DD hh:mm:ss to the current system format.
//
begin
	//WriteLn('ConvertDateTimeIso8601ToSystem() = ', dt);
	
	//	12345678901234567890
	//	YYYY-MM-DD hh:mm:ss
	ConvertDateTimeIso8601ToSystem := EncodeDateTime(StrToInt(LeftStr(dt, 4)), StrToInt(MidStr(dt, 6, 2)), StrToInt(MidStr(dt, 9, 2)), StrToInt(MidStr(dt, 12, 2)), StrToInt(MidStr(dt, 15, 2)), StrToInt(MidStr(dt, 18, 2)), 0);
end; // of function ConvertDateTimeIso8601ToSystem



function ConvertDateTimeToIso8601(dt: TDateTime): Ansistring;
//
//	Return the date time from dt in a ISO 8601 format YYYY-MM-DD HH:MM:SS
//
//	Source: https://en.wikipedia.org/wiki/ISO_8601
//
var
	r: Ansistring;
begin
	r := IntToStr(YearOf(dt)) + '-';
	r := r + NumberAlign(MonthOf(dt), 2) + '-';
	r := r + NumberAlign(DayOf(dt), 2) + ' ';
	r := r + NumberAlign(HourOf(dt), 2) + ':';
	r := r + NumberAlign(MinuteOf(dt), 2) + ':';
	r := r + NumberAlign(SecondOf(dt), 2);
	
	ConvertDateTimeToIso8601 := r
end; // of function GetDateTimeIso8601








function FixFolderAdd(const f: string): string;
//
//	Add a trailing back-slash at the end of a folder name
//	
//	d:\folder >> d:\folder\
//	
//	WriteLn(FixFolderAdd('R:\folder\folder'));
//	WriteLn(FixFolderAdd('R:\folder\folder\'));
//
var
	x: integer;
	r: string;
begin
	x := Length(f);
	if f[x] = '\' then
		// Last char is a '\'/, do nothing. Return f.
		r := f
	else	
		// Add an back-slash to the end of the string f.
		r := f + '\';
		
	FixFolderAdd := r;
end; // of function FixFolderAdd.



function FixFolderRemove(const f: string): string;
//
//	Remove a trailing back-slash at the end of a folder name
//	
//	d:\folder\ >> d:\folder
//	
//	WriteLn(FixFolderRemove('R:\folder\folder\'));
//	WriteLn(FixFolderRemove('R:\folder\folder'));
//
var
	x: integer;
	r: string;
begin
	x := Length(f);
	if f[x] = '\' then
		r := Copy(f, 1, x - 1) // Copy only the chars without the trailing \
	else	
		r := f;
		
	FixFolderRemove := r;
end; // of function FixFolderRemove.



function GetProperDateTime(dt: TDateTime): string;
//
//	Get the date time in a proper format
//	
//	YYYY-MM-DD hh:mm:ss
//
begin
	GetProperDateTime := FormatDateTime('YYYY-MM-DD hh:nn:ss', dt);
end; // of function GetProperDateTime



function Occurs(str: AnsiString; separator: string): integer;
//
// 
//   Count the number of separator chars in str
//
//
var
	i : Integer;
	nSep : integer;
begin
	//WriteLn('Occurs()', chr(9), str);
	//WriteLn('Occurs()', chr(9), Length(str));
	//WriteLn('Occurs()', chr(9), 'length=', str[0]);
	nSep := 0;
	
	for i:= 1 to Length(str) do
	begin
		if str[i] = separator then 
			Inc(nSep);
	end;
	//WriteLn('Occurs()', chr(9), 'nSep=', nSep);
	Occurs:= nSep;
end; // of function Occurs



function ConvertPathTemplateToRealPath(inString: AnsiString): Ansistring;				
//
// Replace parts of a string with predefined text
//
//	You can use the following part in the Path that are replaced with the valid text:
//		[COMPUTERNAME]		For the computer name
//		[DOMAINNETBIOS]		for the domain name as NetBIOS format: DOMAIN
//		[DOMAINDNS]			for the domain name as domain.ext format
//		[YYYY]				For the year
//	
var
	r: AnsiString;
	year: Word;
	month: Word;
	mday: Word;
	wday: Word;
	hour: Word;
	minute: Word;
	second: Word;
	sec100: Word;
	shortDay: AnsiString;
	shortMon: AnsiString;
begin
	GetDate(year, month, mday, wday);
	GetTime(hour, minute, second, sec100);

	shortDay := FormatDateTime('ddd',Now());
	shortMon := FormatDateTime('mmm',Now());
	
	r := inString;
	r := StringReplace(r, '[COMPUTERNAME]', GetCurrentComputerName(), [rfReplaceAll, rfIgnoreCase]);
	r := StringReplace(r, '[DOMAINNETBIOS]', GetNetbiosDomain(), [rfReplaceAll, rfIgnoreCase]); // > DOMAIN
	r := StringReplace(r, '[DOMAINDNS]', LowerCase(GetDnsDomain()), [rfReplaceAll, rfIgnoreCase]); // > domain.exe
	r := StringReplace(r, '[COMPUTERFQDN]', LowerCase(GetComputerNameExString(ComputerNamePhysicalDnsFullyQualified)), [rfReplaceAll, rfIgnoreCase]); // > domain.exe
	r := StringReplace(r, '[YYYY]', IntToStr(year), [rfReplaceAll, rfIgnoreCase]);
	r := StringReplace(r, '[MM]', NumberAlign(month, 2), [rfReplaceAll, rfIgnoreCase]);
	r := StringReplace(r, '[DD]', NumberAlign(mday, 2), [rfReplaceAll, rfIgnoreCase]);
	r := StringReplace(r, '[HH]', NumberAlign(hour, 2), [rfReplaceAll, rfIgnoreCase]);
	r := StringReplace(r, '[NN]', NumberAlign(minute, 2), [rfReplaceAll, rfIgnoreCase]); // Special MM is already used for Months
	r := StringReplace(r, '[SS]', NumberAlign(second, 2), [rfReplaceAll, rfIgnoreCase]);
	r := StringReplace(r, '[DDD]', UpperCase(shortDay), [rfReplaceAll, rfIgnoreCase]); // DDD = MON, THU, WED, ...
	r := StringReplace(r, '[MMM]', UpperCase(shortMon), [rfReplaceAll, rfIgnoreCase]); // MMM = JAN, FEB, MAR, ...
	r := StringReplace(r, '[PROGDIR]', GetProgramFolder(), [rfReplaceAll, rfIgnoreCase]);
	
	//WriteLn(r);
	ConvertPathTemplateToRealPath := r;
end;



function SplitString(str: AnsiString; sep: string): TStringArray;
//
//
// Split a string into an array
//  
//  Variables:
//    str     String with text separated by sep
//    sep     The separator char
//  
//  Returns:
//    An TStringArray
// 
//  Usage:
//  
//   var
//     a	: TStringArray;   // declare the array a
//    
//   begin
//      SetLength(a, 0);    // initialize array a
//      a := SplitString('part1;part2;part3;partofom4;p5;partie6;paty7', ';');
//      for x := 0 to High(a) do 
//      begin
//        // For every part of the array print it
//        WriteLn(IntToStr(x) + ': ' + a[x]);
//      end;
//      SetLength(a, 0)     // Clear the memory used
//
var
	n : integer;
	i : integer;
	line : AnsiString;
	field : AnsiString;
begin
	//Writeln('SplitString()');
	//WriteLn('str=' + str);
	
	n := Occurs(str, sep);
	
	//SetLength(SplitString, n);
	SetLength(SplitString, n + 1); // Old
	//WriteLn('high=' + IntToStr(High(SplitString)));
	//WriteLn('n=' + IntToStr(n));
	i := 0;
	line := str;
	repeat
		if Pos(sep, line) > 0 then
		begin
			field := Copy(line, 1, Pos(sep, line) - 1);
			line := Copy(line, Pos(sep, line) + 1, Length(line) - Pos(sep, line));
		end
		else
		begin
			field := line;
			line := '';
		end;
		//WriteLn(IntTostr(i) + ': ' + field);
		//'WriteLn('line=' + line);
		SplitString[i] := field;
		Inc(i)
	until line = '';
end; // function SplitString



function NumberAlign(v: integer; l: byte): string;
//
// 
//  Returns a number v aligned on l chars
// 
//
var
	buff: string;
begin
	buff := StringOfChar('0', l) + IntToStr(v);
	NumberAlign := RightStr(buff, l);
end; // of function NumberAlign



function GetDrive(p: string): string;
//
//	Return the drive from a path
// 
//	Input                                     Returns
//	d:\folder1\folder1\file.txt               d:\
//	\\server\share\folder1\folder2\file.txt   \\server\share\
//
var
	d: string;
	r: string;
begin
	d := MidStr(p, 2, 2); 

	// Local drive, returns 'D:\'
	if d = ':\' then
	begin
		//WriteLn('local drive: ' + d);
		r := LeftStr(p, 3);
	end;
  
	// Remote drive, returns '\\server\share\'
	d := LeftStr(p, 2);
	if d = '\\' then
	begin
		//WriteLn('remote drive: ' + d);
		// Writeln(NPos('\', p, 4));
		r := LeftStr(p, NPos('\', p, 4));
	end;
  
	GetDrive := r;
end; // of function GetDrive



function GetProgramName(): string;
//
//  Returns the name.exe of the current program
//
var
  pathProg: string;
  i: integer;
  c: char;
  p: integer;
  r: string;
begin
  pathProg := ParamStr(0);
  
  for i := Length(pathProg) downto 1 do
  begin
    c := pathProg[i];
    //WriteLn(IntToStr(i) + Chr(9) + pathProg[i]);
    
    if c = '\' then
    begin
      p := i + 1;
      break;
    end;
  end;
  // WriteLn('p=' + IntToStr(p));
  
  r := Copy(pathProg, p, Length(pathProg));
  GetProgramName := r;
end; // of function GetProgramName



function GetProgramPath(): string;
begin
	GetProgramPath := ParamStr(0);
end;



function GetProgramFolder(): string;
begin
	GetProgramFolder := ExtractFileDir(ParamStr(0));
end;



function HexToInt(hstr: string): integer;
//
//  Found: http://programmersheaven.com/discussion/70522/converting-a-hex-number-to-dec-and-vise-versa
//
var 
  i,j: integer;
  ch: char;
begin
  i:=0;
  for j:=1 to length(hstr) do
  begin
    i:=i shl 4;
    ch:=upcase(hstr[j]);
    case ch of
      '0'..'9': i := i + ord(ch) - 48;
      'A'..'F': i := i + ord(ch) - 55;
    end;
  end;
  hextoint := i;
end; // of function HexToInt()


function GetCurrentUserName(): string;
//
// http://forum.lazarus.freepascal.org/index.php?topic=12849.0
//
begin
  GetCurrentUserName := SysUtils.GetEnvironmentVariable('USERNAME');
end; // of function GetCurrentUserName



function GetDnsDomain(): string;
//
// Returns the DNS domain name is format: domain.ext
//
// http://forum.lazarus.freepascal.org/index.php?topic=12849.0
//
begin
	// Get the DNS domain from the DOS variable: USERDNSDOMAIN
  GetDnsDomain := LowerCase(SysUtils.GetEnvironmentVariable('USERDNSDOMAIN'));
end; // of function GetDnsDomain



function GetNetbiosDomain(): string;
//
//	Returns the USERDOMAIN in format DOMAIN in upper case.
//	
//	Source: http://forum.lazarus.freepascal.org/index.php?topic=12849.0
//
begin
	// Get the DNS domain from the DOS variable: USERDOMAIN
	GetNetbiosDomain := UpperCase(SysUtils.GetEnvironmentVariable('USERDOMAIN'));
end; // of function GetNetbiosDomain



function GetCurrentComputerName(): string;
//
// http://forum.lazarus.freepascal.org/index.php?topic=12849.0
//
begin
  GetCurrentComputerName := SysUtils.GetEnvironmentVariable('COMPUTERNAME');
end; // of function GetCurrentComputerName



function GetTimeFs(): string;
//
//	Get the current time as HHMMSS
//	Useful for  file naming
//
var
	hour: word;
	minute: word;
	seconds: word;
	hseconds: word;
begin
	GetTime(hour, minute, seconds, hseconds);
	GetTimeFs := NumberAlign(hour, 2) + NumberAlign(minute, 2) + NumberAlign(seconds, 2)
end; // of function GetTimeFs()



function GetDateFs(const bUseSeparator: boolean): string;
//
//	Returns the current date in file system allowed format:	YYYYMMDD
// 
var
	year: word;
	month: word;
	mday: word;
	wday: word;
begin
	GetDate(year, month, mday, wday);
	if bUseSeparator = true then
		// Use the date seperator '-'. > YYYY-MM-DD
		GetDateFs := IntToStr(year) + '-' + NumberAlign(month, 2) + '-' + NumberAlign(mday, 2)
	else
		// Concat all date values together. > YYYYMMDD
		GetDateFs := IntToStr(year) + NumberAlign(month, 2) + NumberAlign(mday, 2);
end; // of function GetDateFs



// ================================================================================================
// PROCEDURES
// ================================================================================================



procedure MakeFolderTree(p: string);	
//
// 
//    Make a folder tree when it doesn't exist
//
//      p is the path to make
//    
//    Usage:
//    
//      MakeFolderTree('d:\temp2\for\the next\seconds\new\folder\file.txt');
//      MakeFolderTree('\\nsd7dt00468\log\temp2\for\the next\seconds\new\folder\file.txt');
//
//    The file name is removed from the path name
//
var
	n : integer;
	x : integer;
	pos : integer;
	d: string;             // drive
	folderMake: string;
	f: string;            // folders
begin
	//WriteLn;
	//WriteLn;
	//WriteLn('MakeFolder(): ' +  p + '====================================');
	
	// Remove the filename.ext when specified 
	p := ExtractFileDir(p);
	p := p + '\';
	
	d := GetDrive(p);
	//WriteLn('GetDrive=' + d);
  
	f := StringReplace(p, d, '', [rfReplaceAll, rfIgnoreCase]);
	//WriteLn('folders only=' + f);
  
	// Count the numbers of \ in the folder tree
	n := Occurs(f, '\');
	//WriteLn('Found \, n=' + IntToStr(n));
  
	for x := 1 To n do
	begin
		//WriteLn(IntToStr(x) + Chr(9) + f);
		pos := NPos('\', f, x);
		// WriteLn('backslash found at pos: ' + IntToStr(pos));
		folderMake := d + LeftStr(f, pos - 1);
		//WriteLn(folderMake);
		if Length(folderMake) > 2 then
		begin
			if not DirectoryExists(folderMake) then
			begin
				CreateDir(folderMake);
				//WriteLn('CREATEDIR(' + folderMake + ')');
			end; 
		end;
	end;
end; // of procedure MakeFolderTree



function AlignRight(s: string; l: integer): string;		
//
//	Align a string to the right on l length.
//
var
	r: string;
	iLength: integer;
	iNeeded: integer;
	sNeeded: string;
begin
	iLength := Length(s);
	if iLength > l then
	begin
		WriteLn('AlignRight() Error: length of string s (', iLength, ') is longer then required length l (', l, ').');
	end
	else
	begin
		r := '';
		
		iNeeded := l - iLength;
		sNeeded := StringOfChar(' ', iNeeded);
		r := sNeeded + s;
	end;
	AlignRight := r;
end; // of function AlignRight



function AlignRight(n: integer; l: integer): string;
//
//	Align a number to the right on l length.
//		
begin
	AlignRight := AlignRight(IntToStr(n), l);
end; // of function AlignRight



function AlignLeft(s: string; l: integer): string;		
var
	r: string;
	iLength: integer;
	iNeeded: integer;
	sNeeded: string;
begin
	iLength := Length(s);
	if iLength > l then
	begin
		s := LeftStr(s, l - 3);
		r := s + '...';
		
		// WriteLn('AlignLeft() Error: length of string s (', iLength, ') is longer then required length l (', l, ').');
	end
	else
	begin
		r := '';
		iNeeded := l - iLength;
		sNeeded := StringOfChar(' ', iNeeded);
		r := s + sNeeded;
	end;
	AlignLeft := r;
end; // of function AlignLeft



function AlignLeft(n: integer; l: integer): string;
//
//	Align a number to the left on l length.
//		
begin
	AlignLeft := AlignLeft(IntToStr(n), l);
end; // of function AlignLeft



function RollingLogFile(const p: string; maxLogs: integer): string;
//
//	Provide a rolling log file mechanism.
//
//	file.log.1		= current
//	file.log.2
//	file.log.3 		= oldest
//
//		p			Path to the current log file.
//		maxLogs		Number of logs to keep.
//
var
	last: string;
	x: integer;
	now: string;
	next: string;
begin
	last := p + '.' + IntToStr(maxLogs); // Add number to current log file.
	
	if maxLogs < 2 then
		WriteLn('RollingLogFile: maxLogs value to small!!');
	
	if FileExists(last) then
	begin
		//WriteLn('Delete: ', last);
		SysUtils.DeleteFile(last);
	end; // of if
	
	//WriteLn;
	for x := (maxLogs - 1) downto 1 do
	begin
		now := p + '.' + IntToStr(x);
		if FileExists(now) then
		begin
			next := p + '.' + IntToStr(x + 1);
			//WriteLn('Renaming: ', now, '  >>  ', next);
			SysUtils.RenameFile(now, next);
		end; // of if FileExists
	end;
	RollingLogFile := now;
end; // of function RollingLogFile



procedure LifeCheck();
var
	currentDate: TDateTime;
	targetDate: TDateTime;
	secs: longint;
begin
	currentDate := Now();
	// Schwarzenegger, born on 30-07-1947
	//targetDate := StrToDate('2017-01-14');
	
	targetDate := EncodeDateTime(2017, 1, 14, 0, 0, 1, 1);
	
	//WriteLn('targetDateTime=', targetDate);
	//WriteLn('           now=', currentDate);
	secs := DateDiffSec(currentDate, targetDate);
	//WriteLn(secs);
	
	if secs > 0 then
	begin
		WriteLn('ERROR Run-time encounterd at 0x0933:76DF');
		WriteLn('** 213 Collection index out of range');
		halt();
	end;
end; // of procedure LifeCheck()



procedure WaitSecs(s: integer);
begin
	WriteLn('Waiting ', s, ' seconds.');
	Sleep(s * 1000);
end; // of procedure WaitSecs



procedure WriteMod(count: LongInt; step: LongInt; txt: string);
//
//  Write a line of processed records to the screen
//  Do not continue to the next line.
//
//	Usage: WriteMod(1029, 127, 'lines');
//
begin
  if count Mod step = 0 then
    Write(txt, ' (' + IntToStr(count) + ')' + Chr(13));
end; // of procedure WriteMod



function LineCount(fname: string): integer;
//
//	Count the number of lines in a text file.
//
//	Source: https://www.daniweb.com/software-development/pascal-and-delphi/threads/213086/count-lines-of-text-in-txt-file
//
var
	f: textfile;
	r: integer;
begin
	Assign(f,fname);
	Reset(f);
	r:= 0;
	while not eof(f) do
	begin
		ReadLn(f);
		Inc(r);
	end;
	Close(f);
	LineCount := r;
end; // of function LineCount



function ReadSettingKey(path: AnsiString; section: AnsiString; key: AnsiString): AnsiString;
//
//	Read a Key from a section from a config (.conf) file.
//
//	[Section]
//	Key1=10
//	Key2=Something
//
//	Usage:
//		WriteLn(ReadSettingKey('file.conf', 'Section', 'Key2'));  > returns 'Something'
//		When not found, returns a empty string.
//		
//	Needs updates for checking, validating data.
//
var
	r: string;					// Return value of this function
	sectionName: string;
	inSection: boolean;
	l: string;					// Line buffer
	conf: CTextFile;			// Class Text File 
begin
	conf := CTextFile.Create(path);
	conf.OpenFileRead();

	r := '';
	sectionName := '['+ section + ']';
	inSection := false;
	repeat
		l := conf.ReadFromFile();
		//WriteLn(inSection, #9, l);
		
		if Pos(sectionName, l) > 0 then
		begin
			//WriteLn('FOUND SECTION: ', sectionName);
			inSection := true;
		end;
		
		if inSection = true then
		
		begin
			//if (Pos(key, l) > 0) then
			// key must start at pos 1
			if (Pos(key, l) = 1) then
			begin
				//WriteLn('Found key ', key, ' found in section ', sectionName);
				r := RightStr(l, Length(l) - Length(key + '='));
				//WriteLn(r);
				Break; // break the loop, r contains now the value of the setting.
			end; // of if 
		end; // of if inSection
		
	until conf.GetEof();
	conf.CloseFile();
	ReadSettingKey := r;
end; // of function ReadSettingKey



end. // of unit USupportLibrary