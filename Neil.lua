-- <License Block>
-- Neil.lua
-- Neil
-- version: 20.07.31
-- Copyright (C) 2020 Jeroen P. Broks
-- This software is provided 'as-is', without any express or implied
-- warranty.  In no event will the authors be held liable for any damages
-- arising from the use of this software.
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictions:
-- 1. The origin of this software must not be misrepresented; you must not
-- claim that you wrote the original software. If you use this software
-- in a product, an acknowledgment in the product documentation would be
-- appreciated but is not required.
-- 2. Altered source versions must be plainly marked as such, and must not be
-- misrepresented as being the original software.
-- 3. This notice may not be removed or altered from any source distribution.
-- </License Block>
-- Creation of library
local _Neil = {}
_Neil.Neil = "Neil"
_Neil.UseFileTable = { lua = function(s,chunk) return (loadstring or load)(s,chunk) end }
_Neil.UseDirectoryTable = { }
_Neil.FileSystemCaseSensitive = false -- Must be set to true if directly reading from a Linux system or an other case senstive underground


-- keywords and operators
local operators = {"++","--","!=" --[[Neil will replace != with ~=]],"~=","::=","+=","-=","==","<=",">=","//","||","&&" --[[ Maybe // is a bit odd to call an operator, but for Neil that's the easier way to work]], "/*", "*/",
                    "+", "-", "=", ">",  "<", "!" --[[Neil will replace ! with 'not']] , "%","#","*","/","^",",","/"}
local keywords = { "void","int","byte","number","bool","boolean","delegate","function","plua", "userdata", -- types
                   "switch","case","default", "fallthrough", -- casing
				   "repeat","until","forever","loopwhile", -- basic looping
				   "do","end", -- Basic scope 
				   "if","then","else","elseif", -- if
				   "while", -- while
				   "for", -- for
				   "cfor", -- reserved. Idea for c-syntax for like this "for (i=1;i<=10;i++)".  But not planned for short term
				   "global","public","private","final","abstract", "get", "set", -- needed for declarations
				   "class","module","group","quickmeta",
				   "mod", -- Neil will replace that with % -- Just a nice thing for BASIC and Pascal coders
				   "return", -- We need that one, don't we?
				   "true", "false", -- The two boolean values
				   "nil", "null", -- "null" will be replaced by "nil"

}

local UsedByNeil = {}

-- debug
local debugchat = true
local function Chat(...) 
	if debutchat then return end
	for _,d in ipairs { ... } do print("DEBUG>",d) end
end

-- Constant/ReadOnly
local ReadOnlyWrite = {}

-- Check Type
local CTCase
local function ConvType(v,wanttype,key)  
  local kk = ""
  if key then
     kk = " for "..key
  end
  CTCase = CTCase or {
      byte = function(v)        
           _Neil.Assert(type(v)=="number","Number required"..kk)
           v = math.floor(v+.5)
           if v<0 then
             return 255-(math.abs(v)%255)
           else
             return v%255
           end
         end,
       int = function(v)
         _Neil.Assert(type(v)=="number","Number required"..kk)
         return math.floor(v+.5)
       end,
       ['number'] = function(v)
         _Neil.Assert(type(v)=="number","Number required"..kk)
         return v
       end,
       ['bool'] = function(v)
         _Neil.Assert(type(v)=="boolean","Boolean required"..kk)
         return v
        end,
       ['boolean'] = function(v) return CTCase[wanttype].bool(v) end,
       ['table'] = function(v)
          _Neil.Assert(type(v)=="table","Table required"..kk)
          return v
         end,
       ['userdata'] = function(v)
          _Neil.Assert(type(v)=="userdata","UserData required"..kk)
          return v
         end,
       ['delegate'] = function(v)
          _Neil.Assert(type(v)=="function","Delegate required"..kk)
          return v
       end,   
       ['string'] = function(v)
          if type(v)=="nil" then
             return "nil"             
          elseif type(v)=="string" then   
             return v
          elseif type(v)=="table" and v[".neilclass"] and v[".contains"]("ToString") then
             return v.ToString()
          else
             return tostring(v)
          end
       end,
       var = function(v) return v end
  }
  -- TODO: Class-checktype
  if not CTCase[wanttype] then
	-- TODO: Class check
	print("WARNING! type "..wanttype.." cannot yet be fully processed yet!")
	return tostring(v)
  end
  
  return CTCase[wanttype](v)
end



-- Error
function _Neil.Error(a)
    error(ConvType(a,"string"))
end

function _Neil.Assert(condition,err)
	err = err or "Neil Assertion Failed"
	if not condition then _Neil.Error(err) return false,err else return condition end
end

-- Globals
local substr = string.sub
local Globals
Globals = {
	['LUA'] = { Type='table', Value=_G, Constant=true },
	['GLOBALDUMP'] = { Type='delegate', Constant=true, Value=function() local ret="" for k,v in pairs(Globals) do ret = ret .. k .. " = "..tostring(v) end end },
	['GLOBALEXISTS'] = {Type='delegate', Constant=true, value=function(n) 
		_Neil.Assert(type(n)=="string","Global exists expects a string. Not a "..type(n))
		n = n:upper()
		local g = Globals[n]
		return g~=nil and g~=false
	end},
	['SOUT'] = {Type='delegate', Constant=true,Value=function(...) 
			local ret = ""
			for _,v in ipairs{...} do ret = ret .. Globals.TOSTRING.Value(v) end
			return ret
		end},
	['COUT'] = {Type='delegate', Constant=true,Value=function(...) io.write(Globals.SOUT.Value(...)) end },
	['TOSTRING'] = {Type='delegate', Constant=true,Value=function(v) return ConvType(v,"string") end },
	["REPLACE"] = {Type='delegate', Constant=true,Value=string.gsub },
	['TRIM'] = {Type='delegate', Constant=true,Value=function(str) return (Neil.Globals.ToString(str):gsub("^%s*(.-)%s*$", "%1")) end },
	['LEFT'] = {Type='delegate', Constant=true, Value=function(s, l) 
			if not _Neil.Assert(type(s)=="string","String exected as first argument for 'left'") then return end
			l = l or 1
			_Neil.Assert(type(l)=="number","Number expected for second argument in 'left'")
			return substr(s,1,l)
		end},
	['RIGHT'] = {Type=delegate, Constant=true, Value=function(s,l)
			local ln
			local st
			ln = l or 1
			st = s or "nostring"
			return substr(st,-ln,-1)
		end},
	['MID'] = {Type=delegate, Constant=true, Value=function(s,o,l)
			local ln
			local of
			local st
			ln=l or 1
			of=o or 1
			st=s or ""
			return substr(st,of,(of+ln)-1)
		end},
	['EXTRACTEXT'] = {Type='delegate', Constant=true,Value=function(str,tolower)
		local ret = ""
		local l=0
		local right = _Neil.Globals.Right
		local left = _Neil.Globals.Left
		repeat
			l = l + 1
			if l>=#str then return "" end
			ret = right(str,l)
			if left(ret,1)=="/" or left(ret,1)=="\\" then return "" end
		until left(ret,1)=="."
		if tolower then ret = ret:lower() end
		return right(ret,#ret-1)
	end},
	["SPLIT"] = {Type='delegate', Constant=true, Value=function(inputstr,sep)
        if sep == nil then
                sep = "%s"
        end
        local t = {}
        local i=1
        for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
                t[i] = str
                i = i + 1
        end
        return t
	end},
	["PREFIXED"] = {Type='delegate', Constant=true, Value=function(str,prefix)
		return _Neil.Globals.Left(str,#prefix)==prefix
	end},
	["SUFFIXED"] = {Type='delegate', Constant=true, Value=function(str,suffix)
		return _Neil.Globals.Right(str,#suffix)==suffix
	end},
	["SPAIRS"] = {Type='delegate', Constant=true, Value=function(t, order)
		-- collect the keys
		local keys = {}
		local t2  = {}
		for k,v in pairs(t) do keys[#keys+1] = k  t2[k]=v end
			-- if order function given, sort by it by passing the table and keys a, b,
			-- otherwise just sort the keys 
			if order then
				function bo(a,b) 
					return order(t, a, b) 
				end
				table.sort(keys, bo)
			else
			table.sort(keys)
		end
		-- return the iterator function
		local i = 0
		return function()
			i = i + 1
			if keys[i] then
				return keys[i], t2[keys[i]]
			end
		end
	end},
}
_Neil.Globals = setmetatable({},{
     __index = function(s,k)
          local uk = k:upper()
          -- Chat(uk,tostring(Globals[uk]))
          _Neil.Assert(Globals[uk],"Reading unknown global identifier \""..k.."\"!")
          -- print(uk) for k,v in pairs(Globals[uk]) do print(k,v) end -- debug line --
		  -- print(s,k,uk,Globals[uk],Globals[uk].Value)
          return Globals[uk].Value
        end,
     __newindex = function(s,k,v)
          local uk = k:upper()
          local want = Globals[uk]
          _Neil.Assert(want,"Defining unknown global identifier \""..k.."\"!")          
          _Neil.Assert(not want.Constant,k.." is a constant and cannot be overwritten")
          _Neil.Assert((not want.ReadOnly) or (ReadOnlyWrite.Globals),k.." is read-only and cannot be overwritten")
          Globals[uk].Value = ConvType(v,Globals[uk].Type,k)
        end,
      __call=function(s,newk,oftype,rw,defaultvalue)
          local uk = newk:upper()
          _Neil.Assert(not Globals[uk],"Duplicate global identifier "..newk)
          local newdec = {}
          if oftype=="string" then
              defaultvalue = defaultvalue or ""
          elseif oftype=="number" or oftype=="byte" or oftype=="int" then
              defaultvalue = defaultvalue or 0 
          elseif oftype=="boolean" or oftype=="bool" then
              oftype="bool"
              if defaultvalue==nil then defaultvalue=false end
          end
          newdec.Value = ConvType(defaultvalue,oftype,"New global "..newk)
          newdec.Type = oftype
          newdec.ReadOnly = rw:lower()=="readonly"
          newdec.Constant = rw:lower()=="const" or rw:lower()=="constant"
          Globals[uk] = newdec
        end
})


-- Regular Serialize
local function SafeString(avalue)
		local value = avalue
		for i=1, 255 do
			if i<32 or i>126 or i==34 then value = value:gsub(string.char(i),("\\03d"):format(i)) end
		end
		return value
end

local function Serialize(name,value,tabs)
	tabs = tabs or 0
	_Neil.Assert(type(name)=="string","Parameter #1 for Serialize must be a string and not a "..type(name))
	local t = type(value)
	local ret = "--[["..t.."]] "
	for i=1,tabs do ret = ret .. "\t" end
	if name~="" then ret = ret .. name .. " = " end
	if t == "number" or t=="boolean" then
		ret = ret .. value 
	elseif t=="string" then
		ret = ret .. "\""..SafeString(value).."\""
	elseif t=="userdata" or t=="function" then
		ret = ret .. "\"Type "..t.." cannot be serialized!\""
	elseif t=="table" then
		if t[".neilclass"] then
			ret = ret .. "\"Class serializing not (yet) supported\""
		else
			ret = ret .. "{\n"
			local comma
			for k,v in pairs(value) do
				-- if comma then ret = ret .. "," else comma = true end
				if not (type(k)=="string" or type(k)=="number" or type(k)=="boolean") then return false,"Serialize error! Invalid table key!" end 
				key = k
				if type(k)=="string" then
					key = '"'..SafeString(k)..'"'
				end
				if comma then ret = ret .. ",\n" else comma = true end
				ret = ret .. Serialize("["..key.."]",v,tabs+1)
			end			
			-- for i=1,tabs do ret = ret .. "\t" end 
			ret = ret .. "}"
		end
	else
		ret = ret .. '"Unknown type: '..type(v)..'"'
	end
	return ret
end
Globals.SERIALIZE   = {Type='delegate', Value=Serialize,  Constant=true}
Globals.SAFESTRING  = {Type='delegate', Value=SafeString, Constant=true}





-- Scopes
local Scopes = { [0] = {ID="NOTHINGNESS"} }
local TrueLocals = { NOTHINGNESS = {} }
local Locals = { NOTHINGNESS=TrueLocals.NOTHINGNESS }

-- Incrementors
function _Neil.Inc(value) -- Using "++" will lead to translate to this function
	if type(value)=="number" then
		return value + 1
	elseif type(value)=="table" and value[".neilclass"] and value[".contains"]("_Inc") then
		return value._Inc()
	else
		error("Incrementor not usable on "..type(value))
	end
end

function _Neil.Dec(value) -- Using "--" will lead to translate to this function
	if type(value)=="number" then
		return value + 1
	elseif type(value)=="table" and value[".neilclass"] and value[".contains"]("_Inc") then
		return value._Inc()
	else
		error("Decrementor not usable on "..type(value))
	end
end

function _Neil.Add(value,modvalue) -- Using "+=" will translate to this function
	if type(value)=="number" then
		return value + modvalue
	elseif type(value)=="string" then
		return value .. Neil.Globals.ToString(modvalue)
	elseif type(value)=="table" and value[".neilclass"] then
		Neil.Assert(value[".contains"]("_Add"),"Class "..value[".neilclass"].." has no _Add() method")
		return value._Add(modvalue)
	elseif type(value)=="table" then
		value[#value+1] = modvalue
		return value
	else
		error("Adder not usable on "..type(value))
	end
end

function _Neil.Subtract(value,modvalue) -- Using "-=" will translate to this function
	if type(value)=="number" then
		return value - modvalue
	elseif type(value)=="string" then
		return value:gsub(modvalue,"")
	elseif type(value)=="table" and value[".neilclass"] then
		Neil.Assert(value[".contains"]("_Subtract"),"Class "..value[".neilclass"].." has no _Subtract() method")
		return value._Subtract(modvalue)
	elseif type(value)=="table" then
		local temp = {}
		local array = true
		-- temp stuff and see if this is an array
		for k,v in pairs(value) do
			array = array and (not(type(k)~="number" or (k<=#value and k<1 and k~=math.floor(k))))
			temp[k]=v
		end
		-- Deletion
		for k,_ in pairs(temp) do value[k]=nil end
		if array then
			local i = 1
			for k,v in ipairs(temp) do
				if v~=modvalue then
					value[i]=v
					i = i + 1
				end
			end
		else
			for k,v in pairs(temp) do
				if v~=modvalue then
					value[k]=v
				end
			end
		end
		return value
	else
		error("Subtractor not usable on "..type(value))
	end
end

-- Classes
local function ClassIndex(trueobject,self,key)
end

local function ClassNewIndex(trueobject,self,key,value)
end

local function ClassStaticIndex(class,key)
end

local function ClassStaticNewIndex(class,key,value)
end

local function ClassDestructor(class,key)
end

function _Neil.ClassNew(nameclass)
	-- Does this class exist?
	local ucln = nameclass:upper(); if not _Neil.Assert(Globals[ucln],"No Neil Identifier known as "..nameclass) then return end
	local clss = Globals[ucln]
	if not _Neil.Assert(clss.Type=="class","Requested identiefier is not a class: "..nameclass) then return end

	local trueobject = { ['neilclass']=nameclass}
	local ret = setmetatable({},{__index=function(s,k) return ClassIndex(trueobject,s,k) end, __newindex=function(s,k,v) ClassNewIndex(trueobject,s,k,value) end, __gc=function(s) ClassDestructor(trueobject,s) end })
	return ret
end



-- Load files
local function readAll(file) 
	-- This is when no alternate routine has been set for this
    local f,e = io.open(file, "rb"); _Neil.Assert(f,e); if not f then return nil,"Reading failed!" end
    local content = f:read("*all")
    f:close()
    return content,e
end

local function readDir(dir) 
	-- This is when no alternate routine has been set for this
	-- However Lua does NOT support dir read-outs, but always needs 
	-- an additional API for this! As Neil has been set up not to be 
	-- bound to any engine or OS, you will have to set this yourself.
	-- The function will have to return an array based table in which
	-- all files within a directory are returned... Recursively.
	return nil,"No API has been set up to read entire directories!"
end

function fileExists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

function dirExists(name)
	-- return nil,"No API has been set up to check the existence of directories!"
	return false
end

_Neil.ReadFile   = readAll
_Neil.ReadDir    = readDir
_Neil.FileExists = fileExists
_Neil.DirExists  = dirExists


-- Macro 

function MacroReplace(ori,subk,subw)
	-- NOTE!
	-- I am VERY well aware this routine is SLOOOOOOOW!
	-- I needed a "pure" replacer and not one bound to the rules of REGEX (which the original Lua replacer is) 
	-- and one that does not automatically substitute things it shouldn't (which Lua does as well).
	-- Feel free to replace this with an API in C or something to get it to work in your engine faster.
	-- Also this routine replaces CASE INSENSITVELY. 
	-- I used this slow routine solely to make sure things work without any additional shit.
	-- If you wanna revert back to the original routine simply set Neil.MacroReplace to nil 
	-- and Neil can handle it alright!
	if #subk>#ori then return ori end -- If too small this doesn't make sense anway
	subk = subk:upper()
	local mid = _Neil.Globals.Mid
	local skip = 0
	local ret = ""
	for i=1,#ori do
		local ch = mid(ori,i,1)
		local word = mid(ori,i,#subk):upper()
		if skip>0 then
			-- print("skip:",skip,ch)
			skip = skip - 1
		elseif i<#ori-#subk and word==subk then
			ret = ret .. subw
			skip = #subk - 1
		else
			ret = ret .. ch
			-- print("add:",i,ch,string.byte(ch))
		end
	end	
	return ret
end

_Neil.MacroReplace = MacroReplace

local Macros = {}
local function Macro(script,chunk)
	local LocalMacros = {}
	local lines = _Neil.Globals.Split(script,"\n")
	local trim = _Neil.Globals.Trim
	local prefixed = _Neil.Globals.Prefixed
	local ret = script
	local rep = _Neil.MacroReplace or MacroReplace
	for ln,rl in ipairs(lines) do
		local line = trim(rl)
		-- print(ln,'"'..rl..'"','"'..line..'"')
		if prefixed(line:lower(),"#macro") then
			if not prefixed(line:lower(),"#macro ") then return nil,"Macro syntax error" end
			local chop = _Neil.Globals.Split(line," ")
			if #chop<3 then return nil,"Invalid constructed macro.... Not enough stuff defined... or so it seems! ("..(#chop)..")" end
			if chop[2]=="" or chop[3]=="" then return nil,"Macro could not be properly parsed... Please mind your whitespaces here!" end
			local work
			chop[2] = chop[2]:upper()
			if prefixed(chop[2],"@") then work=LocalMacros else work=Macros end
			--for k,v in pairs(work) do -- This seems an odd way, but as case insensitive macros may be plannend for the future, I need to make sure nothing can happen here!
			--	if k:upper()==chop[2]:upper() then return nil,"Duplicate macro '"..chop2[2].."'" end
			--end
			if work[chop[2]] then return nil,"Duplicate macro '"..chop2[2].."'" end
			work[chop[2]] = ""
			for i,w in ipairs(chop) do
				if i>=4 then work[chop[2]] = work[chop[2]] .. " " end
			    if i>=3 then work[chop[2]] = work[chop[2]] .. w end
			end
		end
	end	
	for _,work in ipairs{ LocalMacros,Macros } do
		for macro,substitute in _Neil.Globals.sPairs(work) do
			-- ret = ret:gsub("\\","\\\\")
			-- ret = ret:gsub(macro,substitute)
			ret = rep(ret,macro,substitute)
		end
	end
	return ret,"All clear!"
end

-- Chop codeup
local function Chop(script,chunk)
	local allowtalk = true -- debug!
	local talk = function(...) 
		for i,l in ipairs {...} do
			print(("talk%04d:>"):format(i),type(l),l)
		end
	end
	local word = {word="", kind=""}
	local instruction = {{words = {word}}}
	local chopped = {instructions=instruction,iused={}}
	local linenumber = 1
	local mid = _Neil.Globals.MID
	local left = _Neil.Globals.LEFT
	local right = _Neil.Globals.RIGHT
	local instring = false
	local incomment = false
	local multiline = false
	local escape = false
	local hexnum = false
	local newword
	local function idins()
		local clean = {}
		local ins = instruction[#instruction]
		local prefixed = _Neil.Globals.Prefixed
		if ins.kind and ins.kind~="" then return end -- No need to waste time on an instruction already identified
		local definition
		for _,w in ipairs(ins.words) do
			if w.word=="++" then
				if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
				definition = "increment"
			elseif w.word=="--" then
				if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
				definition = "decrement"
			elseif w.kind~="" then 
				clean[#clean+1] = w 
			end
			if w.word=="=" then 
				if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
				ins.define = clean
				clean = {}
				definition = "definition"
			elseif w.word == "+=" then
				if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
				ins.define = clean
				clean = {}
				definition = "add"
			elseif w.word == "-=" then
				if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
				ins.define = clean
				clean = {}
				definition = "subtract"
			end
		end
		ins.words = clean
		if #clean==0 or clean[1].kind=="comment" or (prefixed(clean[1].word,"//")) then
			definition = "whiteline"
		elseif clean[1].word=="#" then
			definition = "preprocessor directive"
		else
			definition = definition or "instruction"
		end
		ins.kind = definition
	end
	local function newinstruction() 
		newword()
		idins()
		local ni = {  words = {{word="",kind="" } } } 
		instruction[#instruction+1] = ni
		hexnum=false
	end
	local function newline()
		if not multiline then 
			newinstruction()
			incomment = false
		elseif instring then
			_Neil.Error("Chopping Error: Unfinished string")
			return nil
		end
		linenumber = linenumber + 1
	end
	local function cword()
		return instruction[#instruction].words[#(instruction[#instruction].words)]
	end
	function newword()
		local oldword=cword()
		-- print("Before trim: ",oldword.word)
		oldword.word = _Neil.Globals.Trim(oldword.word)
		-- print("After trim: ",oldword.word)
		if oldword.kind=="identifier" then
			local cw = oldword.word:lower()
			for _,w in ipairs(keywords) do
				if cw==w then oldword.kind="keyword" break end
			end			
		end
		if oldword.kind=="operator" then
			if oldword.word == "!"  then oldword.word="not" oldword.kind="keyword" end
			if oldword.word == "&&" then oldword.word="and" oldword.kind="keyword" end
			if oldword.word == "||" then oldword.word="or"  oldword.kind="keyword" end
		elseif oldword.kind=="keyword" then
			if oldword.word:upper()=="MOD" then oldword.word="%" oldword.kind="operator" end
		end
		oldword.uword = oldword.word:upper()
		oldword.lword = oldword.word:lower()
		instruction[#instruction].words[#instruction[#instruction].words+1] = {word="",kind=""}
		hexnum = false
	end
	local function cinstruction()
		return instruction[#instruction]
	end
	local function haveoperator(s)
		for _,o in ipairs(operators) do
			if o==s then return true end
		end
		return false
	end
		
	local char
	for i=1,#script do
		instruction[#instruction].linenumber = linenumber
		local lastchar = char or "-- NOTHING --"
		char = mid(script,i,1)
		local uchar = char:upper()
		local lchar = char:lower()
		local uasc = uchar:byte()
		-- talk(i,char,lastchar,instring,incomment)
		if char=="\n" then
			newline()
		elseif instring then
			if char=="\"" and (not multiline) and (not escape) then
				instring=false
				newword()
			elseif char=="]" and lastchar=="]" and multiline then
			    instring=false
				newword()
			else
				cword().word=cword().word .. char
			end
		elseif incomment then
			if multiline and char=="/" and lastchar=="*" then incomment=false end
		elseif char==" " or char=="\t" or char =="\r" then -- Ignore whitespaces... With apologies to the developers of the WhiteSpace programming language who thought these characters are valuable :-P
			if cword().word~="" then newword() end
		elseif char==";" then
			local directive
			for i,v in ipairs(instruction[#instructions].words) do
				if v.kind~="" then					
					directive = v.word=="#"
					break
				end
			end
			if not directive then newinstruction() end
		elseif char=="\"" then
			instring=true
			multiline=false
			if cword().word~="" then newword() end
			cword().kind="string"
		elseif (uasc>=65 and uasc<=90) or char=="_" or (((uasc>=48 and uasc<=57) or char==".") and cword().kind=="identifier")   then
			if cword().kind~="identifier" and cword().word~="" then newword() end
			cword().word = cword().word .. lchar
			cword().kind = "identifier"
        elseif (uasc>=48 and uasc<=57) or (char=="." and cword().kind=="number") or (lchar=="x" and cword().word=="0") or (uasc>=65 and uasc<=70 and hexnum) then
			if cword().kind~="number" and cword().word~="" then newword() end
			cword().word = cword().word .. char
			cword().kind = "number"
			if lchar=="x" then hexnum=true end
		elseif char=="!" or char=="%" or char=="#" or char=="^" or char=="*" or char=="(" or char==")" or char=="-" or char=="|" or char=="&" or char=="=" or char=="+" or char=="," or char=="/" or char==">" or char=="<" then
			if cword().kind~="operator" and cword().word~="" then newword() end
			if cword().word~="" then
				-- print(lastchar,char)
				if char=="/" and lastchar=="/" then 
					cword().kind="comment"
					cword().word="irrellevant"
					incomment = true
					multiline = false
				elseif char=="*" and lastchar=="/" then
					cword().kind="comment"
					cword().word="irrellevant (multiline)"
					incomment = true
					multiline = true
				elseif haveoperator(lastchar..char) then				    
					cword().kind="operator"
					cword().word=lastchar..char					
					newword()
				else					
					newword()
					cword().kind="operator"
					cword().word=char
				end
			else
				cword().kind="operator"
				cword().word=char
			end
		else
			_Neil.Error("Chopping error: Unknown character "..char.." in line "..linenumber)
		end
	end
	idins() -- make sure it's ALL DONE
	return chopped
end


-- Translate
function _Neil.Translate(script,chunk)
	-- print("Ori\n",script,"/Ori")
	local macroed,macroerror = Macro(script,chunk); if not macroed then _Neil.Error("Macro error: "..macroerror) return nil,"Macro error: "..macroerror end
	-- print(macroed) error("Crash!") -- debug
	local chopped = Chop(macroed)
	print(Serialize("chopped",chopped))
	return nil,"Sorry translate does not yet work!"
end


-- Load
function _Neil.Load(script,chunk)
	local success,translation = _Neil.Translate(script,chunk)
	_Neil.Assert(succes,"Translation error:\n"..translation)
	if not success then return end -- Safety precaution!
	local ret,err = (loadstring or load)(script,chunk)
	_Neil.Assert(ret,err)
	return ret
end

function _Neil.Use(module,chunk)
	local key,err
	local used
	if _Neil.FileSystemCaseSensitive then
		key = module
	else
		key = module:lower()
	end
	if UsedByNeil[key] then
		return UsedByNeil[key]
	end
	_Neil.ReadFile   = _Neil.ReadFile   or readAll
	_Neil.ReadDir    = _Neil.ReadDir    or readDir
	_Neil.FileExists = _Neil.FileExists or fileExists
	_Neil.DirExists  = _Neil.DirExists  or dirExists
	if     _Neil.FileExists(module..".neil") then used,err =  _Neil.UseFile(module..".neil",chunk) 
	elseif _Neil.DirExists(module..".neilbudle") then used,err = _Neil.UseBundle(module..".neilbundle",chunk) 
	else
		for k,v in pairs(_Neil.UseFileTable) do
			if _Neil.FileExists(module.."."..k) then used,err = _Neil.Assert(v(module.."."..k,ch)) end
		end
		if not used then
			for k,v in pairs(_Neil.UseDirTable) do
				if _Neil.DirExists(module.."."..k) then used,err =  _Neil.Assert(v(module.."."..k,ch)) end
			end
		end
	end
	if not _Neil.Assert(used,"Use failure\n"..err) then return nil,err end
	UsedByNeil[k] = used
	return used
end


-- Warning! Best is to NEVER call this function directly unless you know what you are doing!
function _Neil.UseFile(file,chunk)
	local s,e = _Neil.ReadFile(file)
	if not s then return nil,e end
	return _Neil.Translate(file,chunk)
end

-- Warning! Best is to NEVER call this function directly unless you know what you are doing!
function _Neil.UseDir(dir,chunk)
	local d = _Neil.ReadDir(dir)
	local s = ""
	if _Neil.FileExists(dir.."/_neilbundle.neil") then return _Neil.Use(dir.."/_neilbundle") end
	for _,f in d do
		if _Neil.Globals.Right(f,5)==".neil" then
			s = s .. "#use \"".._Neil.Globals.Left(f,#f-5).."\"\n"
		end
	end
	return _Neil.Load(s,"NEILBUNDLE: "..dir)
end


	
-- Closure
return _Neil