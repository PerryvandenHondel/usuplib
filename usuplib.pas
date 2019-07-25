unit USupLib;


//
// Pascal Unit Support Library (USupLib)
//
// Collection of procedures and functions for general purpose.
//


{$mode objfpc}
{$H+}


interface


// Generic functions
function GeneratePassword(): String;                        // Generate a new password in format AAAAbbbb9999!

// Text functions
function EncloseDoubleQuote(const s: string): string;       // Enclose a string with double quotes (")
function EncloseSingleQuote(const s: string): string;       // Enclose a string with single quotes (')


implementation


function GeneratePassword(): String;
//
//	Generate a new password using chars in S.
//	
//	Format: XXXXxxxx9999!
//
const 
	lowerChar = 'abcdefghijklmnopqrstuvwxyz';
	upperChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
	numberChar = '0123456789';
	specialChar = '!@#$%'; 
var 
	CharCount: integer; 
begin 
    result := '';
	// Initialize the random number generator.
	Randomize;
	
	// Get 4 upper chars.
	for CharCount := 1 to 4 do 
		result := result + upperChar[random(length(upperChar)) + 1];
		
	//result := result + '-';

	// Get 4 lower chars.
	for CharCount := 1 to 4 do 
		result := result + lowerChar[random(length(lowerChar)) + 1];

	//result := result + '-';	

	// Get 4 number chars.
	for CharCount := 1 to 4 do 
		result := result + numberChar[random(length(numberChar)) + 1];

    result := result + specialChar[random(length(specialChar)) + 1];
end; // of function GeneratePassword.


function EncloseDoubleQuote(const s: string): string;
//
//	Enclose the string s with double quotes: s > "s".
//
var
	r: string;
begin
	if s[1] <> '"' then
		r := '"' + s
	else
		r := s;
		
	if r[Length(r)] <> '"' then
		r := r + '"';

	EncloseDoubleQuote := r;
end; // of function EncloseDoubleQuote


function EncloseSingleQuote(const s: string): string;
//
//	Enclose the string s with single quotes: s > 's'.
//
var
	r: string;
begin
	if s[1] <> '''' then
		r := '''' + s
	else
		r := s;
		
	if r[Length(r)] <> '''' then
		r := r + '''';

	EncloseSingleQuote := r;
end; // of function EncloseSingleQuote


end. // of unit usuplib