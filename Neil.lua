-- <License Block>
-- Neil.lua
-- Neil
-- version: 20.12.26
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

-- Debug settings
local showtranslation = false -- When set to true, the translator will show the translation it generated... Only to be used when debugging Neil itself
local usedebug = false


-- Creation of library
local _Neil = {}
_Neil.Neil = "Neil"
_Neil.UseFileTable = { lua = function(s,chunk) return (loadstring or load)(s,chunk) end }
_Neil.UseDirTable = { }
_Neil.FileSystemCaseSensitive = false -- Must be set to true if directly reading from a Linux system or an other case senstive undeunderground

local TranslationCount = 0

-- For Global Defines
local GlobalDefines = { ["SYS_LUA"] = true }

-- forwards
local DefineFunction

-- keywords and operators
local operators = {"++","--","!=" --[[Neil will replace != with ~=]],"~=","::=","+=","-=","==","<=",">=","//","||","&&" --[[ Maybe // is a bit odd to call an operator, but for Neil that's the easier way to work]], "/*", "*/",
					"+", "-", "=", ">",  "<", "!" --[[Neil will replace ! with 'not']] , "%","#","*","/","^",",","/","[","]","&","{","}"}
local keywords = { "void","int","byte","number","bool","boolean","delegate","function","plua", "userdata","string","table", -- types
				   "switch","case","default", "fallthrough", -- casing (fallthrough is reserved... it would require to translate with 'goto', but that would require Lua 5.2 or later)
				   "repeat","until","forever","loopwhile", -- basic looping
				   "do","end", -- Basic scope 
				   "if","then","else","elseif", -- if
				   "while", -- while
				   "for", -- for
				   "ipairs","pairs","in",
				   "cfor", -- reserved. Idea for c-syntax for like this "for (i=1;i<=10;i++)".  But not planned for short term
				   "global","public","private","final","abstract", "get", "set", "local", -- needed for declarations
				   "class","module","group","quickmeta","new","extends",
				   "mod", -- Neil will replace that with % -- Just a nice thing for BASIC and Pascal coders
				   "return", -- We need that one, don't we?
				   "true", "false", -- The two boolean values
				   "nil", "null", -- "null" will be replaced by "nil"
				   "init","defer","div","var","break","and","or","not",
				   "import","export","deftable","defmeta","link","infinity",
				   "try","catch","finally" -- "finally" is not yet supported, but I reserved the word as a keyword for later usage, as I hope to implement it in the future.
}

local declasupport = {"global","public","private","final","abstract", "get", "set", "local","static","const", "readonly" }
local types = {"void","int","string","bool","boolean","delegate","function","userdata","number","table","plua","var","byte"}

local quickmetaparameters = {
	   index = "self, key",
	   newindex = "self, key, value ",
	   call = "self, ...",
	   tostring = "self",
	   len = "self",
	   pairs = "self",
	   ipairs = "self",
	   gc = "self",
	   unm = "self, other",
	   add = "self, other",
	   sub = "self, other",
	   mul = "self, other",
	   div = "self, other",
	   idiv = "self, other",
	   pow = "self, other",
	   mod = "self, other",
	   concat = "self, other",
	   band = "self, other",
	   bor = "self, other",
	   bxor = "self, other",
	   bnot = "self, other",
	   shl = "self, other",
	   shr = "self, other",
	   lt = "self, other",
	   le = "self, other",
	   gt = "self, other",
	   ge = "self, other"

}

local UsedByNeil = {}
local NeilIsUsing = {} -- Has to prevent "circular" uses!
-- debug
local debugchat = true
local function Chat(...) 
	if debutchat then return end
	for _,d in ipairs { ... } do print("DEBUG>",d) end
end

-- Constant/ReadOnly
local ReadOnlyWrite = {}

-- Check Type
local CTCase,ConvType
do local kk
 function ConvType(v,wanttype,key,strict)  
  -- If "strict" is set to 'true' the value 'nil' won't be accepted for string and boollean variables
  -- local kk = ""
  if key then
	 kk = " for "..key
  end
  CTCase = CTCase or {
	  byte = function(v)        
		   _Neil.Assert(type(v)=="number","Number required"..kk)
		   v = math.floor(v+.5)
		   if v<0 then
			 return 255-(math.abs(v)%256)
		   else
			 return v%256
		   end
		 end,
	   int = function(v)
		 _Neil.Assert(type(v)=="number","Number required"..kk.." (Got "..type(v)..")")
		 return math.floor(v+.5)
	   end,
	   ['number'] = function(v)
		 _Neil.Assert(type(v)=="number","Number required"..kk.." (Got "..type(v)..")")
		 return v
	   end,
	   ['bool'] = function(v)
		 _Neil.Assert(type(v)=="boolean","Boolean required"..kk)
		 return v
		end,
	   ['boolean'] = function(v) 
			if strict then _Neil.Assert( v~=nil , "Boolean required... and got 'nil'" ) end
			return CTCase[wanttype].bool(v) 
		end,
	   ['table'] = function(v)
		  _Neil.Assert(type(v)=="table" or v==nil,"Table required"..kk)
		  return v
		 end,
	   ['userdata'] = function(v)
		  _Neil.Assert(type(v)=="userdata" or v==nil,"UserData required"..kk)
		  return v
		 end,
	   ['delegate'] = function(v)
		  _Neil.Assert(type(v)=="function" or v==nil,"Delegate required"..kk)
		  return v
	   end,   
	   ['var'] = function(v) return v end,
	   ['plua'] = function(v) return v end,
	   ['QuickMeta_group'] = function(v) return v end,
	   ['string'] = function(v)
		  if type(v)=="nil" then
			 _Neil.Assert(not strict,"Got nil, but expected a string"..kk)
			 return "nil"             
		  elseif type(v)=="string" then   
			 return v
		  elseif type(v)=="table" and v[".neilclass"] and v[".hasmember"]("ToString") then
			 return v.ToString()
		  else
			 return tostring(v)
		  end
	   end,
	   ['void'] = function() return nil end,
	   var = function(v) return v end
  }
  -- TODO: Class-checktype
  if not CTCase[wanttype] then
	-- TODO: Class check
	print("WARNING! type "..wanttype.." cannot yet be fully processed yet!")
	return tostring(v)
  end 
  
  return CTCase[wanttype](v)
end end

_Neil.ConvType = ConvType

-- Error
function _Neil.Error(a)
	error(ConvType(a,"string").."\n"..debug.traceback () )
end

function _Neil.Assert(condition,err)
	err = err or "Neil Assertion Failed"
	if not condition then _Neil.Error(err) return false,err else return condition end
end

-- Globals
local substr = string.sub
local Globals
Globals = {
	['LUALOADSTRING'] = {Type='delegate', Constant=true, Value=loadstring or load},
	['LUADOSTRING'] = {Type='delegate', Constant=true, Value=function(str,chunk,...) return _Neil.Globals.LuaLoadString(str,chunk)(...) end},
	['NEILLOADSTRING'] = {Type='delegate', Constant=true, Value=function(str,chunk) return _Neil.Load(str,chunk) end},
	['NEILDOSTRING'] = {Type='delegate', Constant=true, Value=function(str,chunk,...) return _Neil.Load(str,chunk)(...) end},
	['NEILUSE'] = {Type='delegate', Constant=true, Value=function(uf,chunk) return _Neil.Use(uf,chunk) end},
	['SETMETATABLE'] = {Type='delegate', Constant=true, Value=setmetatable},
	['TABLECONTAINS'] = {Type='delegate', Constant=true, Value=function(tab,want) 
		for i,v in ipairs(tab) do 
			if v==want then return i end
		end
		return nil
	end},
	['LUAVERSION'] = {Type='delegate', Constant=true, Value = function() 
		local ret,_ = _VERSION:gsub("Lua ","") 
		return tonumber(ret) 
	end},
	['EXPAND'] = {Type='delegate', Value=function (t,p)
		assert(type(t)=="table" or type(t)=="string","Table or string expected in Expand request (got "..type(t)..")\n"..debug.traceback())
		p = tonumber(p) or 1                                 
		if p<#t then 
			if type(t)=="string" then
				return t:sub(p,p),Globals.EXPAND.Value(t,p+1)   
			else
				return t[p],Globals.EXPAND.Value(t,p+1) 
			end
		end
		if p==#t then 
			if type(t)=="string" then
				return t:sub(p,p),Globals.EXPAND.Value(t,p+1)   
			else
				return t[p] 
			end      
		end
		return nil                                 
	end, Constant=true },
	['LUA'] = { Type='table', Value=_G, Constant=true },
	["TRANSLATION_TARGET"] = {Type="string", Value="Lua", Constant=true},
	["CHR"] = { Type='delegate', Value=string.char, Constant=true },
	['ASSERT'] = {Type=='delegate', Value=_Neil.Assert, Constant=true },
	['GLOBALDUMP'] = { Type='delegate', Constant=true, Value=function() local ret="" for k,v in pairs(Globals) do ret = ret .. k .. " = "..tostring(v.Value).."\n" end return ret end },
	['GLOBALEXISTS'] = {Type='delegate', Constant=true, Value=function(n) 
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
	['PRINT'] = {Type='delegate', Constant=true, Value=function(...) print(...) end },
	['SPRINTF'] = {Type='delegate', Constant=true, Value=string.format },
	['PRINTF'] = {Type='delegate', Constant=true, Value=function(f,...) _Neil.Assert(type(f)=="string","Format value must be string") 
		-- _Neil.Globals.Cout(_Neil.Globals.SPrintF(f,...)) 
		local s,e = pcall(_Neil.Globals.SPrintF,f,...)
		if not s then
			local err = e.."\nPrintF(\""..f.."\""
			for i,k in ipairs{...} do err = err..", "..k end
			err=err..")\n\n"..debug.traceback()
			error(err)
		else
			Globals.COUT.Value(e)
		end
	end},
	['CONVTYPE'] = {Type='delegate', Constent=true, Value=ConvType },
	['TOSTRING'] = {Type='delegate', Constant=true,Value=function(v) return ConvType(v,"string") end },
	["REPLACE"] = {Type='delegate', Constant=true,Value=string.gsub },
	["RIPAIRS"] = {type="delegate", Constant=true, Value=function(tab)
		assert(type(tab)=="table","RIPairs requires a table. Got "..type(tab))
		local i = #tab + 1
		return function ()
			  i = i - 1
			  if i<=0 then return nil,nil end
			  return i,tab[i]
		end 
	end},
	['TRIM'] = {Type='delegate', Constant=true,Value=function(str) return (_Neil.Globals.ToString(str):gsub("^%s*(.-)%s*$", "%1")) end },
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
	["EACH"] = {Type='delegate', Constant=true, Value=function(tab)
		assert(type(tab)=="table","EACH expects a table. Got "..type(tab))
		if tab[".neilclass"] then return tab.__EACH() end
		local i = 0
		return function()
			i = i + 1
			if i>#tab then return nil end
			return tab[i]
		end
	end},
	["TYPE"] = {Type='delegate', Constant=true, Value=function(v)
		-- stuff for classes and things, come later!
		return type(v)
	end},
	["LGGTTT"] = {Type='delegate', Constant=true, Value=function(pref,performkill) -- Lua Grab Global Turn To Table
		local ret = {}
		local kill = {}
		for k,v in pairs(_G) do
			if _Neil.Globals.Prefixed(k,pref.."_") then
				kill[#kill+1] = k
				local s = _Neil.Globals.split(k,"_")
				local prevlevel = ret
				for i=2,#s do
					local tg = s[i]:upper()
					if i<#s then
						prevlevel[tg] = prevlevel[tg] or {}
						prevlevel = prevlevel[tg]
					else
						prevlevel[tg] = v
					end
				end
				-- prevlevel = v
			end
		end
		if performkill~=false then for _,k in ipairs(kill) do _G[k]=nil end end
		return ret
	end},
	["TABLETYPECHAIN"] = {Type='delegate', Constant=true, Value=function(tab)
		local ret=""
		for i,v in ipairs(tab) do
			if i>1 then ret=ret.."," end
			ret = ret .. type(v)
		end
		if ret=="" then return "nothing" end
		return ret
	end},
}

local function imodtoglob(name,asname)
	local module = {}
	for k,v in pairs(_G[name:lower()]) do module[k]=v end
	Globals[(asname or name):upper()] = { Type='Lua Module', Constant=true, Value=setmetatable({},{
		__newindex=function() error("Writing to internal module "..name.." prohibited") end,
		__index=function(self,key) 
			local lkey=key:lower() 
			if module[lkey] then return module[lkey] end
			if module[key] then return module[key] end
			error("Module "..name.." does not appear to have an element named "..key)
		end
	})}
end

for k,m in pairs {"Math","IO","OS",MString = "String", MTable = "Table", "CoRoutine", "Debug", "Package" } do
	if _G[m:lower()] then 
		if type(k)=="number" then imodtoglob(m) else imodtoglob(m,k) end
	end
end
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
		  if want.Constant and v == want.Value then return end -- This prevents some static conflicts
		  _Neil.Assert(not want.Constant,k.." is a constant and cannot be overwritten")
		  _Neil.Assert((not want.ReadOnly) or (ReadOnlyWrite.Globals),k.." is read-only and cannot be overwritten")
		  Globals[uk].Value = ConvType(v,Globals[uk].Type,k)
		  --print("Defined global ",uk," with ",v," became ",ConvType(v,Globals[uk].Type,k))
		  if want.UndefinedConstant then
			 want.Constant = true
			 want.UndefinedConstant = nil
		  end
		  if want.UndefinedReadOnly then
			 want.ReadOnly = true
			 want.UndefubedReadOnly = false
		  end
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


-- Locals

local function LocalToFunction(table,key,func)
	local want	
	want            = table[key:upper()]; assert(want,"INTERNAL ERROR!\nlocal table finding error >> '"..key.."'\n\n")
	want.Value      = func
	want.ReturnType = want.Type
	want.Type       = 'delegate'
	want.Constant   = true
end	

local function Local_Index(table,key)
	local uk = key:upper()
	if key==".converttofunction" then return function(name,func) return LocalToFunction(table,name,func) end end
	_Neil.Assert(table[uk],"Reading unknown local identifier \""..key.."\"!")
	 return table[uk].Value
end

local function Local_NewIndex(table,key,value)
	  local uk = key:upper()
	  local want = table[uk]	  
	  _Neil.Assert(want,"Defining unknown local identifier \""..key.."\"!")          
	  _Neil.Assert(not want.Constant,key.." is a constant and cannot be overwritten")
	  _Neil.Assert((not want.ReadOnly) or (ReadOnlyWrite.Globals),key.." is read-only and cannot be overwritten")
	  table[uk].Value = ConvType(value,table[uk].Type,k)
	  if want.UndefinedConstant then
			 want.Constant = true
			 want.UndefinedConstant = nil
	  end
end

local function Local_Call(table,newk,oftype,rw,defaultvalue,strict)
		  local uk = newk:upper()
		  rw = rw or "readwrite"
		  _Neil.Assert(not table[uk],"Duplicate global identifier "..newk)
		  local newdec = {}
		  if oftype=="string" then
			  if strict then _Neil.Assert(defaultvalue,"String value expected for new local "..newk) end
			  defaultvalue = defaultvalue or ""
		  elseif oftype=="number" or oftype=="byte" or oftype=="int" then
			  if strict then _Neil.Assert(defaultvalue,"Numberic value expected for new local "..newk.." (received: "..tostring(defaultvalue)..")") end
			  defaultvalue = defaultvalue or 0 
		  elseif oftype=="boolean" or oftype=="bool" then
			  oftype="bool"
			  if defaultvalue==nil then defaultvalue=false end
		  end
		  -- print(defaultvalue)
		  newdec.Value = ConvType(defaultvalue,oftype,"new "..oftype.." local "..newk)
		  newdec.Type = oftype
		  newdec.ReadOnly = rw:lower()=="readonly"
		  newdec.Constant = rw:lower()=="const" or rw:lower()=="constant"
		  table[uk] = newdec
end

function _Neil.CreateLocals()
	local truetable = {}
	return setmetatable({},{
		__index     = function(s,k)   return Local_Index(truetable,k) end,
		__newindex  = function(s,k,v) return Local_NewIndex(truetable,k,v) end,
		__call      = function(s,...) return Local_Call(truetable,...) end
	})
end	


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
		ret = ret .. tostring(value)
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
	elseif type(value)=="table" and value[".neilclass"] then 
		local w,s = value[".hasmember"]("__Inc")
		if not w then error("Class or object does not have __Inc") end
		return value.__Inc()
	else
		error("Incrementor not usable on "..type(value))
	end
end

function _Neil.Dec(value) -- Using "--" will lead to translate to this function
	if type(value)=="number" then
		return value - 1
	elseif type(value)=="table" and value[".neilclass"] then 
		local w,s = value[".hasmember"]("__Dec")
		if not w then error("Class or object does not have __Dec") end
		return value.__Dec()
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
		Neil.Assert(value[".hasmember"]("_Add"),"Class "..value[".neilclass"].." has no _Add() method")
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
		Neil.Assert(value[".hasmember"]("_Subtract"),"Class "..value[".neilclass"].." has no _Subtract() method")
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

-- Classes Usage
local ClassIndex,ClassNewIndex

local function ClassDestructor(class,key)
end

local function ClassStaticConstructorCall(class,actclass)
	-- print("start") for k,v in pairs(class) do print( "Class has "..type(v).." "..k ) end print("end")
	if class.StaticConstructor and (not class.StaticConstructorRun) then 
		class.StaticConstructorRun=true 
		class.AllowReadOnly = true
		class.StaticConstructor.Value(actclass.Value) 
		class.AllowReadOnly = false
	end
end

local function ClassNew(class,actclass,...)
	  ClassStaticConstructorCall(class,actclass)	  
	  for k,v in pairs(class.AbstractGetProperties) do error("Abstract get-property ("..k..") found in class!") end
	  for k,v in pairs(class.AbstractSetProperties) do error("Abstract set-property ("..k..") found in class!") end
	  for k,v in pairs(class.AbstractMethods)       do error("Abstract method ("..k..") found in class!") end
	  local obj,trueobject	  
	  trueobject = {}
	  for k,odata in pairs({class.Members, class.GetProperties, class.SetProperties, class.Methods }) do 
		  -- trueobject[k] = {}
		  for k2,v2 in pairs(odata) do 
			trueobject[k2]={}
			--print(k,odata,k2,v2)
			for k3,v3 in pairs(v2) do
				-- print("Copy: ",k,k2,k3,v3)
				trueobject[k2][k3]=v3
			end
		  end
	  end
	  trueobject.destructor = class.Destructor
	  trueobject.class=class
	  trueobject.actclass=actclass
	  obj = setmetatable({},{
		  __index = function(s,k) return ClassIndex(trueobject,obj,k) end,
		  __newindex = function(s,k,v) return ClassNewIndex(trueobject,obj,k,v) end,
		  __call = function(s,...) if class.Methods.ONCALL then return class.Methods.ONCALL.Value(obj,...) else error("Class has no \"OnCall\" method") end end,
		  __len = function(s,...) if class.Methods.__LENGTH then return class.Methods.__LENGTH.Value(obj,...) else error("Class has no \"__Length\" method") end end,
		  __pairs = function(s) if class.Methods.__PAIRS then return class.Methods.__PAIRS.Value(obj)() else error("Class has no \"__Pairs\" method") end end,
		  __ipairs = function(s) if class.Methods.__IPAIRS then return class.Methods.__IPAIRS.Value(obj)() else error("Class has no \"__IPairs\" method") end end,
		  __gc = function(s,...) if trueobject.destructor then trueobject.destructor.Value(obj) end end
		  
	  })
	  if trueobject.class.Constructor then
		 trueobject.AllowReadOnly = true
		 trueobject.class.Constructor.Value(obj,...)
		 trueobject.AllowReadOnly = false
	end
	return obj
end



local function ClassStaticIndex(class,actclass,k)	
	ClassStaticConstructorCall(class,actclass)
	if type(k)=="number" then 
		return ClassStaticIndex(class,actclass,"__NumberIndex")(k)
	end
	if k==".neilclass" then
		return true
	elseif k==".hasmember" then
		return function (m)
			m = m:upper()
			if class.Members[m] then return "Member","Regular" end
			if class.StaticMembers[m] then return "Member","Static" end
			if class.GetProperties[m] or class.SetProperties[m] then return "Property","Regular" end
			if class.StaticGetProperties[m] or class.StaticSetProperties[m] then return "Property","Static" end
			if class.AbstractGetProperties[m] or class.AbstractSetProperties[m] then return "Property","Abstract" end
			if class.Methods[m] then return "Method","Regular" end
			if class.StaticMethods[m] then return "Method","Static" end
			if class.AbstractMethods[m] then return "Method","Abstract" end
			return nil,nil
		end
	elseif Globals.PREFIXED.Value(k,".") then
		error("Command field unknown: "..k)
	else
		local uk = k:upper(0)
		if class.Members[uk] or class.GetProperties[uk] or class.Methods[uk] then error("Member "..k.." is not static\n\n"..debug.traceback()) end
		if class.StaticMembers[uk] then return class.StaticMembers[uk].Value end
		if class.StaticMethods[uk] then 
			-- for k,v in pairs(class.StaticMethods[uk]) do print(type(v),k) end
			-- for k,v in pairs(actclass) do print("Class:",type(v),k) end
			class.StaticMethods[uk].TrueMethod = class.StaticMethods[uk].TrueMethod or function(...) return class.StaticMethods[uk].Value(actclass.Value,...) end
			return class.StaticMethods[uk].TrueMethod
		end
		if class.StaticGetProperties[uk] then
			-- for k,v in pairs(actclass) do print("(Set)Class:",type(v),k) end
			return class.StaticGetProperties[uk].Value(actclass.Value)  
		end
		if class.StaticSetProperties[uk] then error("Property '"..k.."' is only configured for writing\n"..debug.traceback()) end -- Safe... The SET has already been acted upon if it exists, and this function was exited!
		error("No static member named '"..k.."' present")
		-- error("Temp Static index error: "..k.." code not yet fully up to date to deal with this request! That's WIP for ya!")
	end
end

local function ClassStaticNewIndex(class,actclass,k,v)
	ClassStaticConstructorCall(class,actclass)
	if type(k)=="number" then
		ClassStaticIndex(class,actclass,"__NumberNewIndex")(k,v)
		return
	end
	if false then -- reserved section for system defintions inside the class
	else
		local uk = k:upper(0)
		if class.Members[uk] or class.SetProperties[uk] or class.Methods[uk] then error("Member "..k.." is not static") end
		if class.StaticMembers[uk] then
			local m = class.StaticMembers[uk]
			if m.Constant then error("Cannot write to constant member: "..k) end
			if m.ReadOnly and (not class.AllowReadOnly) then error("Cannot write to read-only member: "..k) end
			m.Value = ConvType(v,m.Type,"Class member "..k)
			return
		end
		if class.StaticMethods[uk] then error("Cannot overwrite static methods") end
		if class.StaticSetProperties[uk] then
			-- for k,v in pairs(actclass) do print("(Set)Class:",type(v),k) end
			class.StaticSetProperties[uk].Value(actclass.Value,v) return 
		end
		if class.StaticGetProperties[uk] then error("Property '"..k.."' is only configured for reading\n"..debug.traceback()) end -- Safe... The SET has already been acted upon if it exists, and this function was exited!
		error("No static member named '"..k.."' present")
		-- error("Temp Static newindex error: "..k.." code not yet fully up to date to deal with this request! That's WIP for ya!")
	end
end

function ClassIndex(trueobject,self,k)
  if type(k)=="number" then 
	return ClassIndex(trueobject,self,"__NumberIndex")(k)
  end
  if k:lower()==".neilclass" then
		return true
  elseif k:lower()==".neilclassobject" then
		return true
  elseif k:lower()==".hasmember" then
	local check = function(key) 
		local fstatic = ClassStaticIndex(trueobject.class,trueobject.actclass,".hasmember")
		return fstatic(key)
	end
	return check
  elseif k:lower()==".fromclass" then
		return trueobject.class
  elseif Globals.PREFIXED.Value(k,".") then
		error("No system property known for "..k)
  end
  local what,ctype = ClassStaticIndex(trueobject.class,trueobject.actclass,".hasmember")(k) -- trueobject.class[".hasmember"](key) 
  assert(what,"Object has no member named "..k)
  if ctype=="Static" then
	return trueobject.actclass.Value[k]
  end  
  local uk = k:upper()
  if trueobject.class.Members[uk] then return trueobject[uk].Value end
  if trueobject.class.Methods[uk] then return function(...) return trueobject.class.Methods[uk].Value(self,...) end end
  if trueobject.class.GetProperties[uk] then return trueobject.class.GetProperties[uk].Value(self) end
  if trueobject.class.SetProperties[uk] then error("Property "..k.." appears to be write-only") end
  error("No member named '"..k.."' present") -- Should never happen, but just in case!
end

function ClassNewIndex(trueobject,self,key,value)
	if type(key)=="number" then
		ClassIndex(trueobject,self,"__NumberNewIndex")(key,value)
		return
	end
	local lk,uk = key:lower(),key:upper()
	if false then -- reserved for system functionality later
	elseif Globals.PREFIXED.Value(key,".") then
		error("No system property known for "..key)
	end
	local what,ctype = trueobject.actclass.Value[".hasmember"](key) 
	assert(what,"Object has no member named "..key)
	if ctype=="Static" then
	   trueobject.actclass.Value[key] = value
	   return
	end
  if trueobject.class.Members[uk] then 
	 if trueobject.class.Members[uk].Constant then error("Constants cannot be overwritten") end -- should be impossible, as constants are ALWAYS static, but when some clever whizkids break in, this measure is at least taken!
	 if trueobject.class.Members[uk].ReadOnly and (not trueobject.AllowReadOnly) then error("You cannot overwrite read-only values") end
	 -- for k,v in pairs(trueobject) do print("uk = "..uk,"key = "..k,type(v),value) end
	 trueobject[uk].Value = value
	 return
  end
  if trueobject.class.Methods[uk] then error("Methods cannot be overwritten") end
  if trueobject.class.SetProperties[uk] then return trueobject.class.SetProperties[uk].Value(self,value) end
  if trueobject.class.GetProperties[uk] then error("Property "..k.." appears to be read-only") end
  error("No member named '"..key.."' present") -- Should never happen, but just in case!
end


-- Classes Creation
local Class = {}
_Neil.Class = setmetatable({},{
	__index=function(s,k) 
		if Class[k] then return Class[k] end
		_Neil.Error(_Neil..".Class does not have a member named: "..k)
	end,
	__newindex=function()
		error("READ-ONLY OVERWRITE ALERT!")
	end
	})

local GroupClasses = {}
function Class.Create(name,private,extend)	
	local ret = {}
	ret.Class = { Members={}, Methods={}, GetProperties={},SetProperties={}, AbstractMethods={}, AbstractGetProperties={}, AbstractSetProperties={}, StaticMembers={}, StaticMethods={},StaticGetProperties={},StaticSetProperties={},Final={},Base={}, Extends=extend }
	ret.Base = ret.Class.Base
	ret.Final = ret.Class.Final
	ret.Type = "class"
	ret.Constant = true
	ret.Value = setmetatable({},{
				__call=function(s,...)					
					return ClassNew(ret.Class,ret,...)
				end,
				__index=function(s,k) return ClassStaticIndex(ret.Class,ret,k) end,
				__newindex=function(s,k,v) return ClassStaticNewIndex(ret.Class,ret,k,v) end,
				__len=function(s) return ClassStaticIndex(ret.Class,ret,"__LENGTH")() end
			})	
	if private then 
		GroupClasses[name]=ret 
	else
		if not _Neil.Assert(Globals[name:upper()]=="PLACEHOLDER","I cannot create a class named "..name) then return nil end
		Globals[name:upper()] = ret
		if extend then
			extend = extend:upper()
			if not Globals[extend] then
				error("Request to extend a non-existent class! >> "..extend)
			end
			if Globals[extend].Type~="class" then
				error("Tried to extend a non-class >> "..extend.."  (that is a "..tostring(Globals[extend].Type)..")") -- tostring is to prevent unexplainable errors when 'nil' was given, which is by abusing Neil possible.
			end
			local base = Globals[extend]
			for _,k in pairs  { "Members", "Methods", "GetProperties", "SetProperties" } do
				for iname,data in pairs(base.Class[k]) do
					if data.final and k=="Members" then 
						ret.Class.Final[iname] = true
					else
						ret.Class.Base[iname]= ret.Class.Base[iname] or {tpe=k,dat=data}
						if k=="GetProperties" or k=="SetProperties" then ret.Class.Base[iname].tpe="Properties"  ret.Class.Base[iname][k]=false end
					end
					--print("Copying",k,iname,"from",extend,"into",name:upper(),type(data)) -- DEBUG ONLY!!!
					ret.Class[k][iname]=data
				end
				for iname,data in pairs(base.Class["Abstract"..k] or {} ) do
					ret.Base[iname]={tpe=k,dat=data,abstract=true}
					ret.Class["Abstract"..k][iname]=true
				end
				for iname,data in pairs(base.Class["Static"..k]) do
					ret.Class.Final[iname]=true
					ret.Class["Static"..k][iname] = data
				end
			end
		end
		-- for k,v in pairs(Globals) do print(type(v),k) end -- debug
	end
end

local function GetClass(privateclass,classname)
	assert(classname,"String expected for classname, but a "..type(classname).." was received in stead! "..debug.traceback())
	if privateclass then		
		cl = GroupClasses[classname]
	else
		cl = Globals[classname:upper()]
	end
	assert(cl,"No class found name "..classname)
	assert(cl.Type=="class","Not a class: "..classname)
	return cl,cl.Value
end

function Class.RaDGC(classname)
	cl = GroupClasses[classname]
	assert(cl,"No class found name "..classname)
	GroupClasses[classname]=nil
	return cl.Value
end

function Class.NewMember(privateclass,nameclass,membertype,membername,protection,rw,static,value)
	local cl,vcl = GetClass(privateclass,nameclass)
	assert(not cl.Sealed,"Class is already sealed and ready for usage")
	membername = membername:upper()	
	local t,s = vcl[".hasmember"](membername)
	membertype = membertype:lower()
	if t then error("There is already a "..t:lower().." ("..s..") named '"..membername.."' present in that class") end
	local nm = { Type = membertype }
	if rw=="const" then static=true; nm.Constant=true end	
	if rw=="readonly" then nm.ReadOnly=true end
	-- print("defining ",membertype," => ",value)
	if membertype=="int" then
		nm.Value = math.floor(((tonumber(value) or 0)+.5))
	elseif membertype=="byte" then
		nm.Value = math.floor(((tonumber(value) or 0)+.5)) % 256
	elseif membertype=="boolean" then
		nm.Value=value~=nil and value~=false
	elseif membertype=="string" then
		nm.Value=tostring(value or "")
	elseif membertype=="table" then
		if type(value)~="table" then value=nil end
		nm.Value=value or {}
	else
		nm.Value=value
	end
	if static then
		cl.Class.StaticMembers[membername] = nm
	else 
		cl.Class.Members[membername] = nm
	end
end

function Class.NewMethod(privateclass,nameclass,membertype,membername,protection,rw,static,value)
	local cl,vcl = GetClass(privateclass,nameclass)
	assert(not cl.Sealed,"Class is already sealed and ready for usage")
	local t,s = vcl[".hasmember"](membername)
	-- override
	local isabstract = value=="abstract"
	if t then
		if (isabstract) then error("Abstract cannot be used to override something") end
		if (cl.Final[membername:upper()]) then error("Final element override ("..membername..")") end
		if (cl.Base[membername:upper()]) then
			if cl.Base[membername:upper()].tpe~="Methods" then error(membername.." cannot be overridden as a method!") end
			cl.Base[membername:upper()] = nil
			cl.Class.AbstractMethods[membername:upper()]=nil
			t = false
		end
	end
	-- checkout
	membertype = membertype:lower()
	if t then error("There is already a "..t:lower().." ("..s..") named '"..membername.."' present in that class") end
	assert(isabstract or type(value)=="function","Illegal method: "..type(value))
	local nm = {Type='Method', Constant=true, Value=value, ReturnType=membertype }
	if isabstract then
		if membername:lower()=="constructor" or membername:lower()=="desstructor" then error ("No support yet for abstract constructors and destructors") end
		cl.Class.AbstractMethods[membername:upper()] = nm
	elseif static then
		if membername:lower()=="constructor" then
			assert(not cl.StaticConstructor,"There already is a static constructor!")
			cl.Class.StaticConstructor = nm
			--print("STATIC CONSTRUCTOR PRESENT")
		elseif membername:lower()=="destructor" then
			assert(not cl.StaticDestructor,"There already is a static destructor!")
			cl.Class.StaticDestructor = nm
		else
			cl.Class.StaticMethods[membername:upper()] = nm
		end
	else 
		if membername:lower()=="constructor" then
			assert(not cl.Constructor,"There already is a constructor!")
			cl.Class.Constructor = nm
		elseif membername:lower()=="destructor" then
			assert(not cl.Destructor,"There already is a destructor!")
			cl.Class.Destructor = nm
		else
			cl.Class.Methods[membername:upper()] = nm
		end
	end	
end

function Class.NewProperty(privateclass,nameclass,membertype,membername,protection,rw,static,getorset,value)
	local cl,vcl = GetClass(privateclass,nameclass)
	assert(not cl.Sealed,"Class is already sealed and ready for usage")
	local t,s = vcl[".hasmember"](membername)
	membertype = membertype:lower()
	if getorset:upper()=="GET" then getorset="Get" else getorset="Set" end
	if t then
		if (cl.Final[membername:upper()]) then error("Final element override ("..membername..")") end
		if (cl.Base[membername:upper()]) then
			if cl.Base[membername:upper()].tpe~="Properties" then error(membername.." cannot be overridden as a property!") end
			--cl.Base[membername:upper()] = nil
			if cl.Base[membername:upper()][getorset.."Properties"] then
				error("Dupe override for '"..getorset.."' function in property '"..membername.."'") 
			end
			cl.Base[membername:upper()][getorset.."Properties"]=true
			t = false
		end
	end

	if t then 
		if t:lower()=="property" then
			if cl.Class['Static'..getorset..'Properties'][membername:upper()] or cl.Class[getorset..'Properties'][membername:upper()] then error("Dupe "..getorset:lower().." property "..membername) end			
		else
			error("There is already a "..t:lower().." ("..s..") named '"..membername.."' present in that class") 
		end
	end
	assert(type(value)=="function","Illegal method: "..type(value))
	local nm = {Type='Method', Constant=true, Value=value, ReturnType=membertype }
	if static then		
		cl.Class['Static'..getorset..'Properties'][membername:upper()] = nm
	else 
		cl.Class[getorset..'Properties'][membername:upper()] = nm
	end	
end


function Class.Seal(privateclass,nameclass)
	local cl = GetClass(privateclass,nameclass)
	cl.Sealed = true
end

--[[
function _Neil.Class!New(nameclass)
	-- Does this class exist?
	local ucln = nameclass:upper(); if not _Neil.Assert(Globals[ucln],"No Neil Identifier known as "..nameclass) then return end
	local clss = Globals[ucln]
	if not _Neil.Assert(clss.Type=="class","Requested identiefier is not a class: "..nameclass) then return end

	local trueobject = { ['neilclass']=nameclass}
	local ret = setmetatable({},{__index=function(s,k) return ClassIndex(trueobject,s,k) end, __newindex=function(s,k,v) ClassNewIndex(trueobject,s,k,value) end, __gc=function(s) ClassDestructor(trueobject,s) end })
	return ret
end
]]




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

local function fileExists(name)
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
	local downbyif,allowelse
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
		elseif prefixed(line:lower(),"#use") then
			if not prefixed(line:lower(),"#use \"") then return nil,"#use Syntax error: "..line end
			local i = 7
			local touse = ""
			local mid = Globals.MID.Value
			while mid(line,i,1)~="\"" do
				if i>=#line then return nil,"#use Syntax error (unfinished string): "..line end
				touse = touse .. mid(line,i,1)
				i = i + 1
			end -- the "AS" word is not yet important, that comes later during the chop and translation phases.
			local a,e = _Neil.Use(touse)
			if not a then return nil,"#use error: "..e.." ("..line..")" end
			-- This way all possible macros are covered. Calling _Neil.Use again will just return the same data, as it will be saved in the state from now on.
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

-- Chop code up
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
	local incharchain = false
	local multiline = false
	local escape = false
	local hexnum = false
	local newword
	local pure = false
	local function idins()
		local clean = {}
		local haakjes = 0
		local ins = instruction[#instruction]
		local prefixed = _Neil.Globals.Prefixed
		if ins.kind and ins.kind~="" then return end -- No need to waste time on an instruction already identified
		local definition
		for _,w in ipairs(ins.words) do
			if w.kind~="string" then
				if w.word=="(" then 
					haakjes = haakjes + 1
				elseif w.word==")" then
					haakjes = haakjes - 1
				end
				if w.word=="++" then
					if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
					definition = "increment"
				elseif w.word=="--" then
					if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
					definition = "decrement"
				elseif w.kind~="" then 
					clean[#clean+1] = w 
				end
				--if w.word=="=" and haakjes<=0 then 
				--	if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
				--	ins.define = clean
				--	clean = {}
				--	definition = "definition"
				--[[else]]if w.word == "+=" then
					if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
					ins.define = clean
					ins.define[#ins.define] = nil
					clean = {}
					definition = "add"
				elseif w.word == "-=" then
					if not _Neil.Assert(not definition,"Syntax error in line #"..ins.linenumber) then return end
					ins.define = clean
					ins.define[#ins.define] = nil
					--ins.define = clean
					clean = {}
					definition = "subtract"
				end
			elseif w.kind~="" then
				clean[#clean+1] = w 
			end
		end
		ins.words = clean
		if #clean==0 or clean[1].kind=="comment" or (prefixed(clean[1].word,"//")) then
			definition = "whiteline"
		elseif clean[1].word=="#" and clean[1].kind~="string" then
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
		if oldword.kind~="string" then
			oldword.word = _Neil.Globals.Trim(oldword.word)
		end
		-- print("After trim: ",oldword.word)
		if oldword.kind=="identifier" then
			local cw = oldword.word:lower()
			for _,w in ipairs(keywords) do
				if cw==w then oldword.kind="keyword" break end
			end			
		end
		if oldword.kind=="operator" then
			if oldword.word == "!"  then oldword.word="not" oldword.kind="keyword" end
			-- if oldword.word == "&"  then oldword.word=".."  oldword.kind="operator" end -- This was only a temporary solution
			if oldword.word == "&&" then oldword.word="and" oldword.kind="keyword" end
			if oldword.word == "||" then oldword.word="or"  oldword.kind="keyword" end
		elseif oldword.kind=="keyword" then
			if oldword.word:upper()=="MOD" then oldword.word="%" oldword.kind="operator" end
			if oldword.word:upper()=="INFINITY" then oldword.word="..." oldword.kind="operator" end
		end
		oldword.uword = oldword.word:upper()
		oldword.lword = oldword.word:lower()
		instruction[#instruction].words[#instruction[#instruction].words+1] = {word="",kind=""}
		if oldword.kind=="with-mark" then
			-- print('with-mark',oldword.word)
			if oldword.word=="$" then instruction[#instruction].words[#instruction[#instruction].words].kind = "member" end
			if oldword.word=="$$" then instruction[#instruction].words[#instruction[#instruction].words].kind = "method" end
		end
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
		-- --[[pure test]] print(#(cinstruction().words),char:gsub("\n","<CR>"),mid(script:lower(),i,5):gsub("\n","<CR>"),char=="#" , i<=#script-5 , mid(script:lower(),i,5)=="#pure" , #(cinstruction().words)==1)

		if char=="\n" then
			newline()
		elseif instring then
			-- print(char,escape,multiline)
			if char=="\"" and (not multiline) and (not escape) then
				instring=false
				newword()
			elseif char=="]" and lastchar=="]" and multiline then
				instring=false
				newword()
			elseif char=="\\" and (not escape) and (not multiline) then
				cword().word=cword().word .. "\\"
				escape = true
			else
				cword().word=cword().word .. char
				escape = false
			end
		elseif incharchain then
			-- print(char,escape,multiline)
			if char=="'"  and (not escape) then
				incharchain=false
				newword()
			elseif char=="\\" and (not escape) and (not multiline) then
				escape = true
			else
				if Globals.TRIM.Value(cword().word)~="" then cword().word = cword().word ..", " end
				cword().word=cword().word .. (char:byte())
				escape = false
			end
		elseif incomment then
			if multiline and char=="/" and lastchar=="*" then incomment=false end
		elseif char==" " or char=="\t" or char =="\r" then -- Ignore whitespaces... With apologies to the developers of the WhiteSpace programming language who thought these characters are valuable :-P
			if cword().word~="" then newword() end
		elseif char=="#" and i<=#script-5 and mid(script:lower(),i,5)=="#pure" and #(cinstruction().words)==1  then
			pure = true
			newword()
			cword().word="-- PURE --"
			cword().kind="pure"
			cinstruction().kind="pure"			
		elseif pure then
			--error("PURE FORCE QUIT!")
			cword().word = cword().word .. char
			cword().kind = "pure"
			cinstruction().kind="pure"
			if cword().word=="#endpure" then 
				cword().word = "-- END PURE --"
				pure = false
				newword()
			end
		elseif char==";" then
			local directive
			for i,v in ipairs(instruction[#instruction].words) do
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
		elseif char=="'" then
			incharchain=true
			multiline=false
			if cword().word~="" then newword() end
			cword().kind="character chain"
		elseif (cword().kind=="member" or cword().kind=="method") and ( (uasc>=65 and uasc<=90) or char=="_" or (((uasc>=48 and uasc<=57) or char=="." or char==":") and #(cword().word)>0) ) then
			cword().word = cword().word .. char
			--print('mem')
		elseif (uasc>=65 and uasc<=90) or char=="_" or (((uasc>=48 and uasc<=57) --[[or char=="."]]) and cword().kind=="identifier") then
			--print("fuck:"..cword().kind..":"..cword().word)
			if cword().kind~="member" and cword().kind~="method" and cword().kind~="identifier" and cword().word~="" then newword() end
			cword().word = cword().word .. char
			if cword().kind~="member" and cword().kind~="method" then cword().kind = "identifier"			  end
		elseif (uasc>=48 and uasc<=57) or (char=="." and cword().kind=="number") or (lchar=="x" and cword().word=="0") or (uasc>=65 and uasc<=70 and hexnum) then
			if cword().kind~="number" and cword().word~="" then newword() end
			cword().word = cword().word .. char
			cword().kind = "number"
			if lchar=="x" then hexnum=true end
		elseif char=="." then
			newword()
			if i<#script and mid(script,i+1,1)=="." then
				cword().kind="operator"
				cword().word = ".."
				newword()
			elseif i>1 and mid(script,i-1,1)=="." then
			else
				cword().kind="member"
			end
		elseif char==":" then
			newword()
			cword().kind="method"
		elseif char=="$" then
			if cword().kind=="with-mark" then
				cword().word="$$"
				-- double '$' is for method translation... Neil shouldn't need it, but for quick Lua objects it can be.
			else
				newword()
				cword().word="$"
				cword().kind="with-mark"
			end
		elseif char=="!" or char=="%" or char=="#" or char=="^" or char=="*" or char=="(" or char==")" or char=="-" or char=="|" or char=="&" or char=="=" or char=="+" or char=="," or char=="/" or char==">" or char=="<" or char=="[" or char=="]" or char=="{" or char=="}" then
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
			_Neil.Error("Chopping error: Unknown character "..char.." in line "..linenumber.." ("..(chunk or "Unknown chunk")..")")
		end
	end
	idins() -- make sure it's ALL DONE
	return chopped
end

-- Error Catching
local ExceptLoad
do
	local ExceptionScript = [[ // Exception Class
		Class Exception
			Public ReadOnly String Message
			Public ReadOnly String TraceBack 
			Public ReadOnly Int Count
			Static Int Cnt

			Public Constructor(msg,tb)
				Cnt++
				Count = Cnt
				Message = Msg
			End
		End
	]]

	local ExceptionClass

	function ExceptLoad()
		-- ExceptionClass = ExceptionClass or _Neil.Load(ExceptionScript,"Exception Class")() 
		-- Not a good idea on second though... Let's do this differently
		local count = 0
		Globals.EXCEPTION = { Constant = true, Type = 'class', Value = setmetatable({},{ -- faked class!
			["__index"] = function(s,k) if k:upper()=="COUNT" then return count else error("Unknown index") end end,
			["__len"] = function(s) return s.COUNT end,
			["__newindex"] = function() error("Don't modify read-only stuff!") end,
			["__call"] = function(s,er,tb,info) 
				local realtable = { MESSAGE=er, TRACEBACK=tb, COUNT=count, INFO=info }
				count = count + 1
				return setmetatable({},{
					["__index"] = function(s,k) k = k:upper(); return realtable[k] or error("Unknown Exception member: "..k) end, -- Dirty Lua code, I tell ya!
					["__newindex"] = function(s,k,v) error("I am NOT gonna assign "..type(v).." '"..tostring(v).."' to read-only field "..k..", I'm sorry!") end,
					["__call"] = function(s) return s end, -- Well, I dunno...
					["__len"] = function(s) return realtable.COUNT end
				})
			end -- call
		})}
	end

	function GenInfo(max)
		local level = 0
		local ret = {}
		repeat
			level = level + 1
			if level>(max or 1000) then break end
			local info = debug.getinfo(level)
			if not info then break end
			ret[#ret+1] = info
		until false
		return ret
	end

	function _Neil.Catcher(e)	
		local ret = Globals.EXCEPTION.Value(e,debug.traceback(),GenInfo(1000))
		return ret
	end
end 

-- Translate
local scopes = { [0]={id="GLOBAL",type="GLOBAL",identifiers={}}}
local usedscopeids = {}
local cnt = -1
setmetatable(scopes[0].identifiers,{
	__index = function(s,key)
		local Got = Globals[key:upper()]
		if Got then 
			if Got.Type=="plua" then return Got.PLua end
			return _Neil.Neil..".Globals."..key 
		else 
			return nil 
		end
	end,
	__newindex = function(s,key,value)
		error("FATAL ERROR! READ-ONLY GLOBAL SCOPE CHECK DEFINITION DONE ("..key.."="..tostring(value)..")")
	end
})
local function NewScope(script,scopetype)
	local id
	scopetype = scopetype:lower()
	repeat
		cnt = cnt + 1
		id = string.format("Neil_Scope_%010X_%s",cnt,scopetype:gsub("-","_"))
	until (not usedscopeids[id])
	usedscopeids[id] = true
	scopes[#scopes+1] = {
		id = id,
		type = scopetype,
		identifiers={}
	}
	if scopetype~="declaration" and scopetype~="class" and scopetype~="group" and scopetype~="quickmeta" then
		script = script .. string.format(" local %s_locals = %s.CreateLocals(); _HW%s]]",id,_Neil.Neil,id)
	end
	if Globals.SUFFIXED.Value(scopetype,"function") or scopetype=="init" then script = string.format("%s\tlocal %s_defer;",script,id) end
	return script,scopes[#scopes],id
end

local function CurrentScope()
	return scopes[#scopes],scopes[#scopes].id
end

local function EndScope(script)
	scopes[#scopes] = nil
	return script,scopes[#scopes]
end

local function DeclaHelp(w)
	--print('Test',w)
	for _,ww in ipairs( declasupport ) do 
		--print("Declahelp: ",w,ww)
		-- print("is",w,ww,"?","","=>",w==w)
		if ww==w then 
			-- print("returned true")
			return true 
		end 
	end
	return false
end

local function IsType(w)
	-- if _Neil.Globals.Prefixed(w:lower(),"class.") then return true end
	for _,ww in ipairs(types) do 
		-- print(w,ww,w:lower()==ww)
		if w:lower()==ww then 
			return true 
		end 
	end
	return false
end

local function GetIdentifier(id,unk,unknownprefix,ins)
	local classed = ""
	if not unk then
		error("nil for unknowncheck \n\n "..debug.traceback())
	end
	if (not id) then
		error("Total panic! id=nil!\n\n"..debug.traceback())
	end
	id = id:upper()
	if _Neil.Globals.prefixed(id,"CLASS.") then error("Class recognition NOT yet supported!") end
	for i=#scopes,0,-1 do
		if not scopes[i] then 
			error(string.format("INTERNAL ERROR!\n\nscopeid=%d\nscopes=%d\n\n%s",i,#scopes,Serialize("Scopes",scopes)))
		end
		if not scopes[i].identifiers then
			error("INTERNAL ERROR! PLEASE REPORT!\n\n".."Identiferliess scope\n\n"..Serialize("SCOPES["..i.."]",scopes[i])) 
		end
		local f = scopes[i].identifiers[id]
		if scopes[i].type=="class" or scopes[i].type=="group" or scopes[i].type=="module" then 
		   classed="--[[CLASSED:"..scopes[i].id.."]]" -- This is for classes to put things right!
		end 
		if f then return f end --,"Ok" end
	end
   if scopes[#scopes].type=="script" then 
	  return nil,"Unknown identifier (forwarding not possible in ground scope) '"..id.."'"
   end   
   unk.satisfied = unk.satisfied or {}
   unk.unknown = unk.unknown or {}
   unk.unknown[#unk.unknown+1] = { id=id,prefix=unknownprefix or ""}
   if ins then unk.unknown[#unk.unknown].linenumber = ins.linenumber else unk.unknown[#unk.unknown].linenumber="??" end
   -- error("Unknown identifier: "..id.."\n"..Serialize("Unknows",unk))
   -- error(type(unknownprefix))
   if type(unknownprefix or "")~="string" then error("Internal error: unknownprefix is "..type(unknownprefix).."\n\n"..Serialize('unknownprefix',unknownprefix).."\n\n"..debug.traceback()) end
   return (unknownprefix or "") .. "@"..id.."@"   
end

local function GetWith()
	for i=#scopes,1,-1 do
		if scopes[i].with then return scopes[i].with end
	end
	return _Neil.Neil..".Globals"
end

local function LitTrans(alwaysplua, pluaprefix, ins,pos,endword,unk,unknownprefix)
	local fendword
	local ret = ""
	local TriggerNew = 0
	-- print(debug.traceback())
	-- print("LitTrans:",alwaysplua, pluaprefix,ins.linenumber)
	for i=pos,#ins.words do
		local word = ins.words[i]
		if endword==word.lword then fendword = true break end
		if word.kind == "operator" then
		   if word.word == "!=" then 
			  ret = ret .. " ~= "
		   elseif word.word == "+=" and word.word == "-=" then
			  return nil,"Unexpected "..word.word
		   else
			  ret = ret .. " "..word.word.." "
		   end
		elseif word.kind=="character chain" then
			ret = ret .. " "..word.word.." "
		elseif word.lword == "new" then
			TriggerNew = 2
		elseif word.lword == "div" then
			if Globals.LUAVERSION.Value()<5.2 then return nil,"The 'div' keyword's translation requires Lua 5.2 or higher" end
			ret = ret .. " // " -- Sorry, folks, but I decided to stick with the C method for commenting. This keyword can back this up, and beside you still have floor function
		elseif word.kind == "string" then
		   ret = ret .. ' "'..word.word..'" '
		elseif (word.kind~="string") and IsType(word.word) and (word.kind=="identifier" or word.kind=="keyword") and (TriggerNew<=0) then
			if i>=#ins.words or ins.words[i+1].kind=="comment" then return nil,"Delegate expected" end
			if ins.words[i+1].word~="(" then return nil,"Delegate expects function parameters" end
			local f,err = DefineFunction(ins,i+2,word.word,alwaysplua,pluaprefix) --,alwaysplua,pluaprefix,needself)
			if not f then return nil,"Delegate error: "..err end
			ret = ret .. f
			break
		elseif word.kind == "keyword" then
		   if word.lword == "not" or word.lword=="nil" or word.lword=="and" or word.lword=="or" or word.lword=="true" or word.lword=="false" or word.lword == "ipairs" or word.lword == "pairs" then
			  ret = ret .. " "..word.lword.." "
		   elseif word.lword == "null" then
			  ret = ret .. " nil "		   
		   else
			  --print(Serialize("ins",ins))
			  return nil,"Unexpected keyword: "..word.word
		   end
		elseif word.kind == "member" then
			ret = ret .."."..word.word
		elseif word.kind == "method" then
			ret = ret ..":"..word.word
		elseif (word.word == "$" or word.word=="$$") and word.kind~="string" then
			ret = ret .. GetWith()
		elseif word.kind == "number" then
		   ret = ret .. " "..word.word.." "
		elseif word.kind == "identifier" then
			local g,err = GetIdentifier(word.uword,unk,"",unknownprefix,ins)
			--assert(g,("GetIdentifier('%s',<unk>,'','%s',<ins>) returned nil" ):format(word.uword,unknownprefix))
			if not g then return nil,err end
		   ret = ret .. " "..g.." "
		elseif word.kind == "comment" then
		   ret = ret .. "\t-- comment -- "
		else
		   return nil,"Unexpected "..word.kind.." ("..word.word..") "
		end
		if TriggerNew>0 then TiggerNew = TriggerNew - 1 end
	end
	return ret,"Ok",fendword
	
end

function DefineFunction(instruction,startword,returntype,alwaysplua,pluaprefix,needself)
	-- print("DefineFunction(", instruction,startword,returntype,"alwaysplua:"..tostring(alwaysplua),pluaprefix,needself,")")
	local funcform = "function ("
	local startcheck = ""
	local params = {}
	local pos = startword
	local words = instruction.words
	if needself then
		params[1] = { type="plua", name="self", noprefix=true } 
	end -- Needed for class methods
	while words[pos].word~=")" do
		local param = {}
		params[#params+1] = param
		-- if words[pos].word==")" then break end
		if pos>=#words then 			
			return nil,"Function definition syntax error (incompleteness)" 
		end
		if IsType(words[pos].word) then
			param.type=words[pos].lword
			pos = pos + 1
		end
		if words[pos].kind~="identifier" then return nil,"Function definition syntax error (identifier expected)\n"..Serialize(tostring(pos),words) end
		param.name = words[pos].word
		pos = pos + 1
		if words[pos].word==")" then break end
		if words[pos].word=="=" then 
			pos = pos + 1
			if pos>=#words then return nil,"Function definition syntax error (optional default value incompleteness)" end
			if words[pos].kind=="number" or words[pos].lword=="true" or words[pos].lword=="false" or words[pos].lword=="nil" then 
				param.default = words[pos].word
				pos = pos + 1
			elseif words[pos].kind=="string" then
				param.default = '"'..words[pos].word..'"'
				pos = pos + 1
			elseif words[pos].lword=="null" then
				param.default = "nil"
				pos = pos + 1
			else
				return nil,"Function definition syntax error (constant value expected)"
			end
		end
		if words[pos].word==")" then break end
		if words[pos].word=="," then pos = pos + 1 else return nil,"Function definition syntax error (comma expected)" end
	end
	-- print(_Neil.Globals.Serialize("Parameters",params))
	local tscript,tscope,tid = NewScope("",returntype.."-function")
	if needself then
			tscope.identifiers.SELF = 'self'
	end
	tscope.returntype = returntype
	startcheck = tscript
	for i,param in ipairs(params) do
		if i~=1 then funcform = funcform ..", " end
		funcform = funcform .. "neil_function_arg"..i
		if param.type=="plua" or alwaysplua  then
			if param.noprefix then
				startcheck = startcheck .. "local "..param.name
			else
				startcheck = startcheck .. "local "..pluaprefix..param.name
			end
			if param.type~="plua" then
				if param.default then 
					startcheck = startcheck .. " = ".._Neil.Neil..".ConvType(neil_function_arg"..i.." or "..param.default ..", \""..param.type.."\", \"function argument "..i.."\")"
				else
					startcheck = startcheck .. " = ".._Neil.Neil..".ConvType(neil_function_arg"..i                        ..", \""..param.type.."\", \"function argument "..i.."\")"
				end			
			else
				if param.default then 
					startcheck = startcheck .. " = neil_function_arg"..i.." or "..param.default 
				else
					startcheck = startcheck .. " = neil_function_arg"..i
				end
			end
			startcheck = startcheck .."; "
			if param.noprefix then
				tscope.identifiers[param.name:upper()] = param.name
			else
				tscope.identifiers[param.name:upper()] = pluaprefix..param.name
			end
		else
			local arg = "neil_function_arg"..i
			if param.default then arg = arg .." or "..param.default end
			startcheck = startcheck .. tid.."_locals(\""..param.name.."\",\""..(param.type or "var").."\",'readwrite',"..arg..",true); "
			tscope.identifiers[param.name:upper()] = tid.."_locals."..param.name
			tscope.haslocals = true
		end
	end
	funcform = funcform ..")\t\t "..startcheck
	-- print(funcform) -- debug
	return funcform
end

local function DefineDelegate()
	error("Delegate definition not yet present")
end



local function Declaration(ins,scope,alwaysplua,pluaprefix,localplua)
	local i=1
	local isglobal,rw = "notset","readwrite"
	local identifier
	local initvalue = "nil"
	local istype
	local ret = ""
	local realtype	
	localplua = localplua and scope.kind~="script"
	alwaysplua = alwaysplua or localplua
	-- decladata
	while not IsType(ins.words[i].lword) do
		if i>#ins.words then return nil,"Declaration syntax error" end
		local w = ins.words[i].lword
		if w=="global" or w=="public" then
			if isglobal~="notset" then return nil,"Protection level conflict" end
			isglobal="global"
		elseif w=="local" or w=="private" then
			if isglobal~="notset" then return nil,"Protection level conflict" end
			isglobal="local"
		elseif w=="static" then
			if isglobal~="notset" then return nil,"Protection level conflict" end
			isglobal="static"			
		elseif w=="readonly" or w=="const" then
			if rw~="readwrite" then return nil,"Read-Write setting conflict" end
			rw=w			
		elseif IsType(ins.words[i].word) then
			-- all okay
		else
			return nil,"Unexpected '"..ins.words[i].word.."' in declaration"
		end
		i = i + 1
	end
	-- print("Decladebug:",i,ins.words[i].word)
	if isglobal=="notset" then isglobal = "local" end
	istype = ins.words[i].lword
	if istype=="plua" then
		if isglobal=="static" then
			return nil,"pLua identifiers cannot be static"
		end
		if rw=="readonly" then
			return nil,"pLua identifiers cannot be readonly"
		end
		if rw=="const" then
			return nil,"pLua identifiers cannot be constant"
		end
	end
	-- all complete?
	if i>=#ins.words then return nil,"Incomplete declaration" end
	if isglobal=="notset" then isglobal="local" end
	-- identifier
	i = i + 1
	identifier = ins.words[i].word
	-- verify
	if keywords[identifier:lower()] then return nil,"Unexpected keyword ("..identifier..") in declaration" end
	if isglobal=="global" and Globals[ins.words[i].uword] then 
		return nil,"Duplicate global identifier: "..identifier 
	elseif scope.identifiers[ins.words[i].uword] then 
		-- for k,v in pairs(scope.identifiers) do print(k,v) end
		return nil,"Duplicate local identifier: "..identifier 
	end
	-- All clear... let's get this show on the road
	-- Always Plua
	realtype = istype 
	if alwaysplua and (isglobal~="static") then
		istype="plua"
	end
	-- Declare
	local name
	if isglobal=="static" then
		local staticid = 0
		local cheat
		repeat
			staticid = staticid + 1
			cheat = string.format("__NEIL_STATIC_IDENTIFIER_FOR_SCOPE_%s_ID%010X",scope.id,staticid):upper()
		until not Globals[cheat]
		local new_static = {
			Type=istype,
		}
		Globals[cheat] = new_static
		if rw == "const" then new_static.UndefinedConstant = true 
		elseif rw == "readonly" then new_static.UndefinedReadOnly = true end
		scope.identifiers[identifier:upper()] = "Neil.Globals."..cheat
		name = _Neil.Neil..".Globals."..cheat
	elseif isglobal=="global" then
		local new_global = { Type=istype }
		Globals[identifier:upper()] = new_global
		if rw == "const" then new_global.UndefinedConstant = true 
		elseif rw == "readonly" then new_global.UndefinedReadOnly = true end
		if istype=="plua" then
			new_global.PLua = pluaprefix..identifier
			name = pluaprefix..identifier
		else
			name = _Neil.Neil..".Globals."..identifier
		end
		ret = ret .. " --[[ Global "..identifier.." created]]\t"
	else -- In all other situations, I guess it's a local we got!
		if istype=="plua" then
			name = pluaprefix..identifier
			scope.identifiers[identifier:upper()] = name
			ret = ret .. "local "..name..";\t"
		else
			ret = ret ..  scope.id.."_locals(\""..identifier.."\",\""..istype.."\"," 
			if rw == "const" then ret = ret .. "UndefinedConstant"
			elseif rw == "readonly" then ret = ret .. "UndefinedReadOnly" 
			else ret = ret .. "ReadWrite" end
			scope.identifiers[identifier:upper()] = scope.id.."_locals."..identifier
			ret = ret .. ");\t"
			name = scope.id.."_locals."..identifier
			scope.haslocals = true
		end
	end
	-- ret = ret .. name .. " = "
	-- End of line? Standard value then		
	if i==#ins.words then
		if isglobal=="static" then ret = ret .. "if not __neil_staticsdefined['"..name.."'] then __neil_staticsdefined['"..name.."'] = true; " end
		ret = ret .. name .. " = "
		if realtype=="int" or realtype=="number" or realtype=="byte" then
			initvalue = "0"
		elseif realtype=="string" then
			initvalue = '""'
		elseif realtype=="bool" or realtype=="boolean" then
			initvalue = "false"
		elseif realtype=="table" then
			initvalue = "{}"
		else
			initvalue = 'nil'
		end
		ret = ret .. initvalue
		if isglobal=="static" then ret = ret .. " end " end
	elseif ins.words[i+1].word=="=" then
		if isglobal=="static" then ret = ret .. "if not __neil_staticsdefined['"..name.."'] then __neil_staticsdefined['"..name.."'] = true; " end
		ret = ret .. name .. " = "
		if not ins.words[i+2] then 
			return nil,"Value expected" 
		elseif IsType(ins.words[i+2].word) then
			if isglobal=="static" then return nil,"Static cannot be used for function defintions (also not for delegates) " end
			ret = ret .. DefineDelegate()
		else
			-- print(i,i+2)
			local lt,err=LitTrans(alwaysplua, pluaprefix, ins,i+2,nil,{}) -- must be taken care of later!
			if not lt then return nil,err end
			ret = ret ..lt 
		end
		if isglobal=="static" then ret = ret .. " end " end
	elseif ins.words[i+1].word=="(" then
		if isglobal=="static" then return nil,"Static cannot be used for function defintions" end
		if rw~="readwrite" then return nil,"Read-Write permissions cannot be altered in general function definition" end
		local data,error = DefineFunction(ins,i+2,istype,alwaysplua,pluaprefix)
		if not data then return nil,error end
		if alwaysplua then
			ret = ret .. name .." = " ..
			data
			scope.closure="\t--[[End PLUA function]]"
		else
			if isglobal=="global" then
				local uk = identifier:upper()
				local what = Globals[uk]
				what.Type="delegate"
				what.ReturnType=istype
				what.UndefinedConstant=true
				ret = ret .. _Neil.Neil..".Globals."..identifier.." = "..data
				scopes[#scopes].closure = "--[[ End global function: "..identifier.."]] "
			else
				ret = ret .. scope.id.."_locals['.converttofunction']( '"..identifier.."', ".. data
				scopes[#scopes].closure=")"
			end
			scope.haslocals = true
			-- return nil,"No functions yet"
		end
	end
	-- return nil,"Nothing yet, but from here things SHOULD be okay ("..istype..","..identifier..")"
	return ret,"Ok!"
end

local function NewConditionScope(stype,ins,unk)
	-- endword is only experimental. I am not sure if this will eventually make it in the end product
	local endword
	if stype=="if" or stype=="elseif" then endword="then" elseif stype=="while" then endword="do" else return nil,"Internal error! Unknown Condition scope: "..stype end 
	local res,err,fendword = LitTrans(alwaysplua, pluaprefix, ins,2,endword,unk)
	local ns_s = NewScope("",stype)	 
	return res,ns_s,err
end

local function GetFunctionType()
	local s=_Neil.Globals.Suffixed
	for i=#scopes,1,-1 do
		scope=scopes[i]
		if s(scope.type,"function") or s(scope.type,"method") then
		   assert(scope.returntype,"INTERNAL ERROR! Undefined return-type for function/method. This can only happen due to a bug!\nPlease report!\n"..Serialize("Function Scope Data",scope))
		   return scope.returntype
		end
		if scope.type=="init" or scope.type=="quickmeta-index" then
			return "void"
		end
		if scope.type=="quickmeta-newindex" or scope.type=="script" then
			return "var"
		end
	end
	return "var" -- Should never happen,but just in case!
end

local switches = { oud = {}, nieuw = {} }
local switchmethod

function switches.oud.start(ins,unk)
	local script,scope,scopeid = NewScope("","switch")
	local ret = "do "..script
	if #ins.words<2 then return nil,"Incomplete switch" end
	if ins.words[2].kind == "comment" then return nil,"Illegal comment" end		
	scope.countcases = 0
	scope.casecheck = "switch_check_var_"..scopeid.."_value"
	scope.casestyle = "oud"
	local scr,err = LitTrans(alwaysplua, pluaprefix,ins,2,nil,unk)
	if not scr then return nil,"Switch syntax error: "..err end
	ret = ret .. " local "..scope.casecheck.." = "..scr.."\t"
	return ret,"Ok"
end

function switches.oud.fallthrough()
	return nil,"Fallthrough NOT supported in current switch style"
end

function switches.oud.case(ins,scope)
	local ret = ""
	if #ins.words<2 then return nil,"Incomplete case" end
	if scope.countcases == 0 then ret = "if " else ret = "elseif" end
	if scope.hasdefault then return nil,"'Case' no longer allowed when a 'Default' has been used" end
	for i=2,#ins.words do
		local word = ins.words[i]
		if word.kind=="comment" then break end
		if word.kind~="string" and word.kind~="number" and word.kind~="character chain" and (word.word~="true" or word.word~="false") then return nil,"Case requires complete constant value (got "..word.kind..")" end
		if i~=2 then ret = ret .. " or " end
		ret = ret .. "("..scope.casecheck.." == "
		if word.kind=="string" then ret = ret .. '"' .. word.word .. '"' else ret = ret .. word.word end
		ret = ret .. ")"
	end
	ret = ret .. " then "
	if scope.countcases~=0 then _EndScope() end
	local cscript,cscope,cscopeid = NewScope("","case")
	-- print("Case",CurrentScope())
	ret = ret .. cscript .. "\t"
	scope.countcases = scope.countcases + 1
	return ret,"Ok"
end

function switches.oud.default(ins,scope)
	local ret = ""
	if #ins.words>1 and ins.words[2].kind~="comment" then return nil,"Default does not expect anything" end
	if scope.countcases == 0 then return nil,"Default without prior Cases" end
	ret = ret .. " else "
	_EndScope()
	scope.hasdefault = true
	local cscript,cscope,cscopeid = NewScope("","default")
	ret = ret .. cscript .. "\t"
	return ret,"Ok"
end

function switches.oud.einde(scope)
	if scope.cases == 0 then return nil,"Switch without Case" end
	--error("EINDE")
	--print(CurrentScope())	
	_EndScope() -- print(CurrentScope()) -- yes, this needs to be done TWICE so I need one here!
	return "end end","Ok"
end

function switches.oud.replacer(r) return r end

function switches.nieuw.start(ins,unk)
	local script,scope,scopeid = NewScope("","switch")
	local ret = "" 
	if #ins.words<2 then return nil,"Incomplete switch" end
	do -- comments can now really spook the process up, so I don't want them, at all!
		local cmtremover = {}
		for _,w in ipairs(ins.words) do
			if w.kind~="comment" then cmtremover[#cmtremover+1]=w end
		end
		ins.words = cmtremover
	end
	if #ins.words<2 then return nil,"Incomplete switch" end
	scope.countcases = 0
	scope.casecheck = "switch_check_var_"..scope.id.."_value"
	scope.casestyle = "nieuw"
	local scr,err = LitTrans(alwaysplua, pluaprefix,ins,2,nil,unk)
	if not scr then return nil,"Switch syntax error: "..err end
	ret = ret .. "do "..script.." local "..scope.casecheck.." = "..scr.."\t ***SWITCHREMOVE:"..scope.id.."*** end"
	scope.replacer = ""
	return ret,"Ok"
end

function switches.nieuw.case(ins,scope)
	local ret = ""	
	if #ins.words<2 then return nil,"Incomplete case" end
	if scope.countcases == 0 then scope.replacer = scope.replacer .. "if " else scope.replacer = scope.replacer .. "elseif" end
	if scope.hasdefault then return nil,"'Case' no longer allowed when a 'Default' has been used" end
	for i=2,#ins.words do
		local word = ins.words[i]
		if word.kind=="comment" then break end
		if word.kind~="string" and word.kind~="number"  and word.kind~="character chain"  and (word.word~="true" or word.word~="false") then return nil,"Case requires complete constant value" end
		if i~=2 then scope.replacer = scope.replacer .. " or " end
		scope.replacer = scope.replacer .. "("..scope.casecheck.." == "
		if word.kind=="string" then scope.replacer = scope.replacer .. '"' .. word.word .. '"' else scope.replacer = scope.replacer .. word.word end
		scope.replacer = scope.replacer .. ")"
	end
	scope.replacer = scope.replacer .. " then "
	if scope.countcases~=0 then 
		local casescope,caseid = CurrentScope ()
		--print(Serialize("casescope",casescope)) 
		--print(casescope.fallthrough , casescope.casereturned,not(casescope.fallthrough or casescope.casereturned),ins.linenumber)
		if not( casescope.fallthrough or casescope.casereturned) then ret = ret .. " goto "..scope.id.."__theend " end
		_EndScope() 
		ret = ret .." end " 
	end
	local cscript,cscope,cscopeid = NewScope("","case")
	-- print("Case",CurrentScope())
	local label = scope.id.."__case"..scope.countcases
	ret = ret .. " ::"..label..":: do "..cscript .. "\t"
	scope.replacer = scope.replacer .. " goto "..label.." "
	scope.countcases = scope.countcases + 1
	return ret,"Ok"
end

function switches.nieuw.default(ins,scope)
	local ret = ""
	if #ins.words>1 and ins.words[2].kind~="comment" then return nil,"Default does not expect anything" end
	if scope.countcases == 0 then return nil,"Default without prior Cases" end
	scope.replacer = scope.replacer .. " else goto "..scope.id.."__default"
	do
		local casescope,caseid = CurrentScope()
		if not( casescope.fallthrough or casescope.casereturned) then ret = ret .. " goto "..scope.id.."__theend " end
	end
	_EndScope()
	scope.hasdefault = true
	local cscript,cscope,cscopeid = NewScope("","default")
	local label = scope.id.."__default"
	ret = ret .. "end ::"..label..":: do "..cscript .. "\t"
	return ret,"Ok"
end

function switches.nieuw.fallthrough(scope)
	if scope.fallthrough then return nil,"Dupe fallthrough" end
	scope.fallthrough = true
	return "--[[ Fallthrough request ]]  ","Ok"
end

function switches.nieuw.einde(scope)
	if scope.cases == 0 then return nil,"Switch without Case" end
	if not scope.hasdefault then scope.replacer = scope.replacer .." else goto "..scope.id.."__theend\t" end
	--error("EINDE")
	--print(CurrentScope())	
	_EndScope() -- print(CurrentScope()) -- yes, this needs to be done TWICE so I need one here!
	return "end end ::"..scope.id.."__theend:: ","Ok"
end

function switches.nieuw.replacer(r,scope) 
	--print(Serialize("SwitchScope",scope))
	--error(r.." "..scope.replacer)
	return MacroReplace(r,"***SWITCHREMOVE:"..scope.id.."***",scope.replacer)
end


local function decideswitchmethod()
	if (not switches.nieuw) or Globals.LUAVERSION.Value()<5.2 then return "oud" else return "nieuw" end
end

local function switchhandler(ins,scope)
	if ins.words[1].word=="case" then
		return switches[switchmethod].case(ins,scope)
	else
		return switches[switchmethod].default(ins,scope)
	end
end


local function Translate(chopped,chunk)
	--print("CHOPPED CHUNK: "..(chunk or "FUCK YOU").."\n"..Serialize("",chopped).."\n END CHOPPED")
	local ret,scope,scopeid = NewScope("--[["..(chunk or "?").."]] local ".._Neil.Neil.." = ".._Neil.Neil.." --[[ Neil Translation started "..os.date().."; "..os.time().." ]]\t local __neil_init_functions = {};\tlocal __neil_staticsdefined = {};\t","script")
	local alwaysplua = false
	local localplua = false
	local pluaprefix = ""
	local cline = 1
	local unknowns = {}
	local suffixed = _Neil.Globals.Suffixed
	local allowground
	local cfor = 0
	local regions = 0
	local modules = {}
	local nocatch
	local LocalDefines = {}
	local onmute,onif,allowelse = false,false,true
	local prefixed = Globals.PREFIXED.Value
	
	

	function _EndScope()
		local s,sid = CurrentScope()
		-- print("__Ending Scope: "..sid.." has locals: "..tostring(s.haslocals))
		if not s.haslocals then
			-- print("__No needless local table needed >>>\n",string.format(" local %s_locals = %s.CreateLocals(); --[[]]",sid,_Neil.Neil),string.format("--[[ No locals of scope %s ]]",sid))
			--ret = ret:gsub(string.format("local %s_locals = %s.CreateLocals();",sid,_Neil.Neil),string.format("--[[ No locals of scope %s ",sid))
			ret = ret:gsub(string.format("local %s_locals",sid),string.format("--[[ local %s_locals",sid))
			-- print(ret)
		else
			ret = ret:gsub(string.format("_HW%s",sid),string.format("--[[ %s ",sid))
		end
		EndScope()
	end

	local function UnDefer()
		local i = #scopes
		while (not Globals.SUFFIXED.Value(scopes[i].type,"function")) and scopes[i].type~="init" do 		
			-- print(scopes[i].type)
			if i<=0 or scopes[i].type=="script" then return nil,"Internal error: Can't undefer here" end
			i = i - 1 
		end		
		if not scopes[i].defer then return "--[[ No undefer in scope "..scopes[i].id.."]]\t",nil end
		local deferid = string.format("%s_defer",scopes[i].id)
		return  "if "..deferid.." then for i=#"..deferid..",1,-1 do "..deferid.."[i].func(".._Neil.Neil..".Globals.Expand( "..deferid.."[i].param ) ) end end\t", nil
		
	end


	local mkConstructor
	function ClassParse(insid,ins,scope)
		local cl_private = scope.privateclass == true--scope.classscope ~= "class" and scope.classscope ~= "group"
		if ins.words[1].lword=="end" and ins.words[1].kind=="keyword" then
			ret = ret .. _Neil.Neil..".Class.Seal("..tostring(cl_private)..", \""..scope.classname.."\")\t"
			EndScope()
			if scope.class=="class" then
			   -- nothing
			elseif scope.class=="group" then
			   return "No group seal support yet"
			elseif scope.class=="module" then
				return "No module sealing support yet"
			end
			ret = ret .. "\tend"
			return
		end

		local i=1
		local isglobal,rw = "notset","readwrite"
		local identifier
		local initvalue = "nil"
		local istype
		local isstatic = false
		local isabstract = false
		local property
		-- local ret = ""
		local realtype
		local scopeid = scope.id
		scope.members = scope.members or {}
		local members = scope.members; members.get = members.get or {}; members.set = members.set or {}

		if ins.words[1].lword=="link" then			
			isstatic = true
			if #ins.words<i+3 then return "Incomplete link" end
			if ins.words[i+1].kind~="identifier" then return "Identifier expected to name link" end
			local linkname = ins.words[i+1].uword
			if members[linkname] then  return "Link produced dupe member" end
			if ins.words[i+2].word~="=" then return "= expected to define link" end
			local value,err = LitTrans(alwaysplua, pluaprefix, ins,i+3,nil,unknowns)
			if not value then return err end
			ret = ret .. string.format("\t".. _Neil.Neil..".Class.NewProperty(%s,\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %s,\"%s\",    %s ",cl_private,scope.classname,"var",linkname,isglobal,rw,true,"get","function(s) return "..value.." end) ")
			ret = ret .. string.format("\t".. _Neil.Neil..".Class.NewProperty(%s,\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %s,\"%s\",    %s ",cl_private,scope.classname,"var",linkname,isglobal,rw,true,"set","function(s,v) "..value.." = v end) ")
			return nil
		end
		if not( DeclaHelp(ins.words[1].lword) or IsType(ins.words[1].word) or ins.words[1].lword=="constructor" or ins.words[1].lword=="destructor") then
			print("\x07\x1b[41,34,1mILLEGAL INSTRUCTION\x1b[0m\n"..Serialize("Instruction",ins).."\n"..Serialize("Scope",scope))
			return "Illegal instruction"
		end


		function mkConstructor(dtype)
			assert(dtype,"Internal error: mkConstructor(nil)")
			if property then return "Constructors and Destructors may not be properties" end
			istype="void"
			if i<#ins.words and ins.words[i+1].kind~="comment" then
				if ins.words[i+1].word~="(" then return "Syntax error in "..dtype.." definition" end
				if not ins.words[i+2] then return "Incomplete "..dtype.." definition" end
				if ins.words[i+2]~=")" and (isstatic or dtype=="destructor") then
					return "Destructors and static constructors do not take parameters in line #" .. ins.linenumber
				end
				local f,e = DefineFunction(ins,i+2,istype,alwaysplua,pluaprefix,true)
				ret = ret .. string.format("\t".. _Neil.Neil..".Class.NewMethod(%s,\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %s,    %s ",cl_private,scope.classname,istype,dtype,isglobal,rw,isstatic,f)
				local s,sid = CurrentScope()
				s.closure=")"
				s.returntype="void"
			else
				local script,s,sid = NewScope("function(self)",dtype.."-function")
				ret = ret .. string.format("\t".. _Neil.Neil..".Class.NewMethod(%s,\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %s,    %s ",cl_private,scope.classname,istype,dtype,isglobal,rw,isstatic,script)
				s.closure=")"
				s.returntype="void"
			end
		end

		if scope.classscope=="group" then isstatic=true end

		-- localplua = localplua and scope.kind~="script"
		-- alwaysplua = alwaysplua or localplua
		-- decladata
		while not IsType(ins.words[i].lword) do
			if i>#ins.words then return "Declaration syntax error" end
			local w = ins.words[i].lword
			if w=="constructor" or w=="destructor" then
				return mkConstructor(w)
			elseif w=="global" or w=="public" then
				if isglobal~="notset" then return "Protection level conflict" end
				isglobal="public"
			elseif w=="local" or w=="private" then
				if isglobal~="notset" then return "Protection level conflict" end
				isglobal="private"
			elseif w=="static" then
				-- if isglobal~="notset" then return "Protection level conflict" end
				-- isglobal="static"			
				isstatic = true
			elseif w=="readonly" or w=="const" then
				if rw~="readwrite" then return "Read-Write setting conflict" end
				rw=w
			elseif w=="get" or w=="set" then
				if property then return "Property syntax error" end
				property = w
			elseif w=="abstract" then
				isabstract=true
			elseif IsType(ins.words[i].word) then
				-- all okay
			else
				return "Unexpected '"..ins.words[i].word.."' in declaration"
			end
			i = i + 1
		end
		-- print("Decladebug:",i,ins.words[i].word)
		if isglobal=="notset" then isglobal = "local" end
		istype = ins.words[i].lword
		if istype=="plua" then
			return "The 'plua' type is not compatible with classes, group and modules"
		end
		-- all complete?
		if i>=#ins.words then return "Incomplete declaration" end
		if isglobal=="notset" then isglobal="local" end
		-- identifier
		i = i + 1
		identifier = ins.words[i].word
		-- verify
		if keywords[identifier:lower()] then return "Unexpected keyword ("..identifier..") in member declaration" end
		if members[ins.words[i].uword] then 
			-- for k,v in pairs(scope.identifiers) do print(k,v) end
			return "Duplicate member: "..identifier 
		end
		-- All clear... let's get this show on the road
		-- Always Plua
		realtype = istype 		
		-- Class.NewMember(privateclass,nameclass,membertype,membername,protection,rw,static,value)
		if identifier=="constructor" or identifier=="destructor" then
			return mkConstructor(identifier)
		elseif property then
			local func
			local nscopescript,nscope,nid = NewScope("","property-"..property.."-function")
			if isabstract then return "Abstract not yet supported for properties" end
			if istype=="void" then return "Void type not valid for properties" end
			if property=="get" then				
				func = "function(self)"
				nscope.returntype = istype
			else
				func = "function(self,value) value=".._Neil.Neil..".Globals.ConvType(value,'"..istype.."','property-set-value')"
				nscope.identifiers.VALUE = "value"
				nscope.returntype = "void"
			end
			nscope.identifiers.SELF = "self"
			nscope.closure = ")"
			ret = ret .. string.format("\t".. _Neil.Neil..".Class.NewProperty(%s,\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %s,\"%s\",    %s ",cl_private,scope.classname,istype,identifier,isglobal,rw,isstatic,property,func).."\t"..nscopescript
			scope.identifiers[identifier:upper()] = "self."..identifier
			-- error("Property defintion still WIP")
		elseif i>=#ins.words or ins.words[i+1].kind=="comment" then
			if istype=="void" then return "Void type only allowed for functions and methods" end
			--                                             priv cname,  memtyp  memnam  prot   rw       static value
			if isabstract then return "Invalud use of 'abstract'" end
			ret = ret .. string.format("\t".._Neil.Neil..".Class.NewMember(%s,\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %s,    nil )",cl_private,scope.classname,istype,identifier,isglobal,rw,isstatic)
			-- if isstatic and scope.class=="class" then scope.locals[membername] = _Neil.Neil..".Globals."..classname.."."..membername end
			scope.identifiers[identifier:upper()] = "self."..identifier
		elseif ins.words[i+1].word == "=" then
			if istype=="void" then return "Void type only allowed for functions and methods" end
			local value,err = LitTrans(alwaysplua, pluaprefix, ins,i+2,nil,unknowns) -- (ins,pos,endword,unk,unknownprefix)
			if not value then return err end
			ret = ret .. string.format("\t".. _Neil.Neil..".Class.NewMember(%s,\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %s,    %s )",cl_private,scope.classname,istype,identifier,isglobal,rw,isstatic,value)
			scope.identifiers[identifier:upper()] = "self."..identifier
		elseif ins.words[i+1].word =="(" then
			scope.identifiers[identifier:upper()] = "self."..identifier
			local func,error 
			if isabstract then
				if ifstatic then error "Abstract cannot be used on static methods" end
				func = '"abstract"'
			else
				func,error = DefineFunction(ins,i+2,istype,false,false,true,alwaysplua,pluaprefix)
			end
			if not func then return error end
			ret = ret .. string.format("\t".. _Neil.Neil..".Class.NewMethod(%s,\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %s,    %s ",cl_private,scope.classname,istype,identifier,isglobal,rw,isstatic,func)
			if isabstract then
				ret = ret .. ")"
			else
				scopes[#scopes].closure=")"
			end
			-- error("Method parsing not yet supported")
		end
		-- error("Can't get on the move yet\n"..Serialize("class_ins["..insid.."]",ins).."\n\n"..ret)
	end
	-- </ CLASS PARSE >

	--- <QUICK META>
	function ParseQuickMeta(ins)
		scope.qmparsed = scope.qmparsed or {}
		if #ins.words>=2 and ins.words[2].kind~="comment" then return "QuickMeta multiword error" end		
		local event=ins.words[1].lword
		if event=="end" and ins.words[1].kind=="keyword" then
			_EndScope()
			ret = ret .. "})\t end"
			return
		end
		if scope.qmparsed[event] then return "QuickMeta dupe event: ".. event end
		if not quickmetaparameters[event] then return "Unknown QuickMeta event" end
		scope.qmparsed[event] = true
		ret = ret .. "['__"..event.."'] = function ("..quickmetaparameters[event]..")\t"
		local scr,scp,sid = NewScope("","QuickMeta-"..event.."-event-method-function")
		ret = ret .. scr
		scp.returntype="var"
		scp.closure=","
		scp.identifiers = scp.identifiers or {}
		for _,v in pairs(Globals.SPLIT.Value(quickmetaparameters[event],",")) do
			local tv = Globals.TRIM.Value(v)
			if tv~="..." then scp.identifiers[tv:upper()] = tv end
			-- print("Added: '"..tv.."'")
		end
		-- print(Serialize(event.."-scope",scp))
		-- error("Parsing Quick Meta not yet fully up!")
	end
	--- </QUICK META>
	
	-- <DEFTABLE>
	function DefTableParse(ins,scope)
		if IsType(ins.words[1].lword) then
			return "Function definitions in DefTable not yet possible"
		end
		for _,chk in ipairs(ins.words) do
			if chk.kind=="comment" then
				return "Comments not allowed in DefTable definitions. You can place them on loose lines, but not in actual definitions! Error"
			end
		end
		if ins.words[1].word=="end" and ins.words[1].kind=="keyword" then
			ret = ret .. "}"
			if scope.type=="defmeta" then ret = ret .. ")" end
			ret = ret .. " end"
			EndScope()
			return nil
		end
		if #ins.words == 1 then
			if scope.type=="defmeta" then
				return "Array defintion not possible for defmeta"
			end
			ret = ret .."\""..ins.words[1].word.."\","
			return nil
		end
		if ins.words[2].word == "=" and ins.words[2].kind == "operator" then
			if #ins.words<3 then return "More input expected!" end
			local id = ins.words[1].word
			if scope.type == "defmeta" then 
				id = ins.words[1].lword
				if not _Neil.Globals.Prefixed(id,"__") then id = "__"..id end
			end
			local script,err = LitTrans(alwaysplua, pluaprefix, ins,3,false,unknowns,unknownprefix)
			if not script then return err end
			ret = ret .. '["'..id..'"] = '..script
			local tscope,tscopeid = CurrentScope ()
			if tscope==scope then ret = ret .. "," end
			return nil
		else
			return "DefTable syntax error!"
		end
	end
	-- </DEFTABLE>

	for insid,ins in ipairs(chopped.instructions) do
		if cline ~= ins.linenumber then ret = ret .."\n--[["..(ins.linenumber or "<??>").."]]\t"; cline=ins.linenumber end
		--print(_Neil.Globals.Serialize("ins",ins))
		if ins.kind=="comment" then
		   ret = ret .. "-- comment received --"
		elseif ins.kind=="whiteline" then
		   ret = ret .."-- whiteline received --"
		elseif ins["kind"] == "preprocessor directive" then
			if #ins.words<2 then 
				print(Serialize("???",ins))
				return nil,"Invalid directive ("..chunk..":"..ins.linenumber..")" 
			end
			local dir = ins.words[2].lword -- word 1 is always # after all!
			if dir~="use" then ret = ret .. "-- directive: #"..dir end
			if dir=="macro" then
			   -- Do nothing... Macros have already been taken care off after all!
			elseif dir=="define" then
				for i,w in ipairs(ins.words) do
					if i>2 then
						if w.kind=="comment" then break end
						if prefixed(w.uword,"SYS_") then return nil,"SYS_ prefix reserved for system features in line #"..ins.linenumber.." ("..chunk..")" end
						if prefixed(w.uword,"LOC_") then LocalDefines[w.uword]=true else GlobalDefines[w.uword] = true end
					end
				end
			elseif dir=="undef" then
				for i,w in ipairs(ins.words) do
					if i>2 then
						if w.kind=="comment" then break end
						if prefixed(w.uword,"SYS_") then return nil,"SYS_ prefix reserved for system features in line #"..ins.linenumber.." ("..chunk..")" end
						if prefixed(w.uword,"LOC_") then LocalDefines[w.uword]=nil else GlobalDefines[w.uword] = nil end
					end
				end
			elseif dir=="if" or dir=="elseif" or dir=="elif" then
				if (dir=="if" and onif) or (dir~="if" and (not onif)) then return nil,"Invalid use of #"..dir.." in line #"..ins.linenumber.." ("..chunk..")" end
				onif=true
				allow = true
				if allowelse then
					for i,w in ipairs(ins.words) do 
						if i>2 then
							if prefixed(w.uword,"LOC_") then allow = allow and LocalDefines[w.uword] else allow = allow and GlobalDefines[w.uword] end
							-- print("if:",i,w.uword,allow)
						end
					end
					if allow and dir~="if" then allowelse=false end
					onmute = not allow
				end
			elseif dir=="else" then
				if not onif then return nil,"Invalid use of #"..dir.." in line #"..ins.linenumber.." ("..chunk..")" end
				if allowelse then onmute=not onmute; allowelse=false end
			elseif dir=="endif" or dir=="fi" then
				if not onif then return nil,"Invalid use of #"..dir.." in line #"..ins.linenumber.." ("..chunk..")" end
				onmute,onif,allowelse = false,false,true
			elseif dir=="nocatch" or dir=="notry" then
				if #ins.words==1 or ins.words[2].kind=="comment" then 
					nocatch=true
				else
					nocath = ins.words[2].uword~="OFF" and ins.words[2].uword~="FALSE"
				end
			elseif dir=="mkl_lic" or dir=="mkl_version" then
			   -- These are only for my own usage, and engines will pick those up (if they are configured to do so)
			   -- For Neil itself, these tags have no value at all and should be ignored!
			elseif dir=="region" then
				   regions = regions + 1
			elseif dir=="endregion" then
				   if regions<=0 then return nil,"#endregion without #region in line #"..ins.linenumber.." ("..chunk..")" end
				   regions = regions - 1
			elseif dir=="localplua" then
				if #ins.words<=2 then
					localplua = true
				else 
					localplua = ins.words[3].lword=="on"
				end
			elseif dir=="alwaysplua" then
				if #ins.words<=2 then
					alwaysplua = true
				else 
					alwaysplua = ins.words[3].lword=="on"
				end
				ret = ret .. " -- always plua: "..tostring(alwaysplua)
			elseif dir=="use" then
				if scope.type~="script" then return nil,"#use request may only be in ground scope ("..chunk..", line #"..ins.linenumber..")" end
				if #ins.words<=2 then return nil,"Wordless #use in line #"..ins.linenumber end -- This should never be possible due to the macro parsing that takes place first, but just in case!
				local filename = ins.words[3].word
				local use_as
				if #ins.words>3 then
					if #ins.words~=5 then return nil,"#use Syntax error (too many stuff given!) in line #"..ins.linenumber.." ("..chunk..")" end
					if ins.words[4].lword ~= "as" then return nil,"#use Syntax error (\"as\" expected) in line #"..ins.linenumber.." ("..chunk..")" end
					use_as = ins.words[5].word
				else
					local split = Globals.SPLIT.Value(filename,"/")
					use_as = split[#split]
				end
				use_as = Globals.TRIM.Value(use_as)
				local u_use_as = use_as:upper()
				local allowed = true
				for i=1,#use_as do
					local ch = Globals.MID.Value(u_use_as,i,1)
					local bt = ch:byte()					
					if bt>=48 and bt<=57 then return nil,"#use creates an identifier starting with a number ("..use_as..") in line #"..ins.linenumber.." ("..chunk..")" end
					allowed = allowed and ((bt>=48 and bt<=57) or (bt>=65 and bt<=90) or ch=="_")
					--print(allowed,bt,ch)
				end
				if not allowed then return nil,"#use creates an illegal identifier ("..use_as..") in line #"..ins.linenumber.." ("..chunk..")" end
				-- local u_use_as = use_as:upper()
				-- print(u_use_as,Globals[u_use_as])
				if Globals[u_use_as] and (not Globals[u_use_as].UsedModule) then 
					return nil,"#use creates an duplicate identifier ("..use_as..") in line #"..ins.linenumber.." ("..chunk..")" 
				end
				Globals[u_use_as] = Globals[u_use_as] or {UndefinedConstant=true, Type="var", UsedModule=true, Value=_Neil.Use(filename,filename)}
				-- ret = ret .. _Neil.Neil .. ".Globals."..use_as.." = ".._Neil.Neil ..".Use(\""..filename.."\")"					
				ret = ret .. " -- used "..filename.." as "..use_as.." -- "
			elseif dir=="pluaprefix" then
				if #ins.words<=2 then
					pluaprefix=""
				else
					pluaprefix = ins.words[3].word
				end
			else
				   return nil,"Unknown directive #"..ins.words[2].word.." in line #"..ins.linenumber.." ("..chunk..")" 
			end
		elseif onmute then
			ret = ret .. "-- code muted by #if directive"
		elseif (ins.kind=="pure") then
			local line = ""
			local rep =  _Neil.MacroReplace or MacroReplace
			for _,w in ipairs(ins.words) do if #line>0 then line = line .." " end line = line .. w.word end -- Should always be 1 word, but just in case
			for si = #scopes , 0 , -1 do
				for id,tt in pairs(scopes[si].identifiers) do
					line = rep(line,"{$"..id.."$}",tt)
				end
			end
			ret = ret .. line			
		elseif scope.type=="defmeta" or scope.type=="deftable" then
			local e = DefTableParse(ins,scope)
			if e then return nil,e.." in line #"..ins.linenumber.." ("..chunk..")" end
			scope,scopeid = CurrentScope()			
		elseif scope.classscope then -- must after the preprocessor directives be first!!!
			local e = ClassParse(insid,ins,scope)
			if e then return nil,e.." in line #"..ins.linenumber.." ("..chunk..")" end
			scope,scopeid = CurrentScope()
		elseif ins.words[1].lword=="case" or ins.words[1].lword=="default" then
			local r,e
			if #scopes==1 then return nil,"Grounded case/default in line #"..ins.linenumber.." ("..chunk..")" end
			if scopes[#scopes-1].type=="switch" then 
				r,e = switchhandler(ins,scopes[#scopes-1]) 
				if not r then return nil,e.." in line #"..ins.linenumber.." ("..chunk..")" end
			elseif scopes[#scopes].type=="switch" then 
				r,e = switchhandler(ins,scopes[#scopes]) 
				if not r then return nil,e.." in line #"..ins.linenumber.." ("..chunk..")" end
			else 
				return nil,ins.words[1].word.." without Switch in line #"..ins.linenumber.." ("..chunk..")"  
			end
			ret = ret .. r .."\t"
			scope,scopeid = CurrentScope()
		elseif ins.words[1].lword=="fallthrough" then
			if scope.type~="case" then return nil,"Unexpected '"..ins.words[1].word.."' in line #"..ins.linenumber.." ("..chunk..")" end
			local sws = scopes[#scopes-1]
			local r,e = switches[sws.casestyle].fallthrough(scope)
			if not r then return nil,e.." in line #"..ins.linenumber.." ("..chunk..")" end
			ret = ret .. r
		elseif scope.type=="switch" then
			return nil,"Illegal action in switch scope in line #"..ins.linenumber.." ("..chunk..")" 
		elseif ins.words[1].lword=="switch" then
			switchmethod = switchmethod or decideswitchmethod()
			local r,e = switches[switchmethod].start(ins,unknowns)
			if not r then 
				return nil,e.." in line #"..ins.linenumber.." ("..chunk..")" 
			end
			ret = ret .. " "..r.." "
		elseif scope.type=="quickmeta" then 
			local e = ParseQuickMeta(ins)
			if e then return nil,e.." in line #"..ins.linenumber.." ("..chunk..")" end
			scope,scopeid = CurrentScope()
		elseif ins.words[1].lword == "class" or ins.words[1].lword == "group" then
			if scope.type~="script" then
				return nil,"Classes and groups cannot be created in subscopes ("..chunk..":"..ins.linenumber..")"
			elseif #ins.words<2 then
				return nil,"Incomplete class definition ("..chunk..":"..ins.linenumber..")"
			elseif ins.words[2].kind~="identifier" then
				return nil,"Identifier expected ("..chunk..":"..ins.linenumber..")"
			end
			local classname = ins.words[2].uword
			local script
			if Globals[classname] then
				return nil,"Duplicate global identifier ("..chunk..":"..ins.linenumber..")"
			end
			Globals[classname] = "PLACEHOLDER"
			local extend
			if #ins.words>2 then
				if ins.words[3].lword~="extends" then
					return nil,"Keyword \"extends\" expected ("..chunk..":"..ins.linenumber..")"
				end
				if #ins.words<4 then
					return nil,"Incomplete class extension defitiontion ("..chunk..":"..ins.linenumber..")"
				end
				extend = ins.words[4].uword
			end
			script,scope,scopeid = NewScope("", ins.words[1].lword)
			scope.classscope =  ins.words[1].lword 
			scope.classname = classname
			scope.with = _Neil.Neil..".Globals."..classname
			ret = ret .."do "..script.."\t --[[ Class: "..classname.."]]\t" -- Security measure to create a 'do' scope... Overall it never hurts, and some 'damage' can be prevented on the way.
			if extend then
				ret = ret .. _Neil.Neil..".Class.Create(\""..classname.."\", false, \""..extend.."\")" -- name,private,extend
			else
				ret = ret .. _Neil.Neil..".Class.Create(\""..classname.."\", false)" -- name,private,extend
			end
		elseif ins.words[1].lword == "module" then
			if scope.type~="script" then
				return nil,"Modules cannot be created in subscopes ("..chunk..":"..ins.linenumber..")"
			end
			local mtype="group"
			local id = #modules 
			local modulename,modulereturn,found
			if #ins.words>1 and ins.words[2].lword=="class" then 
				mtype="class" 
				-- print("Recognized as class!") -- debug
			end
			repeat
				id = id + 1
				modulename = ("MOD_%X"):format(id)
				modulereturn = _Neil.Neil..".Class.RaDGC(\""..modulename.."\")"
				found = false
				for _,m in ipairs(modules) do found = found or m==modulereturn end
			until not found
			modules[#modules+1]=modulereturn
			script,scope,scopeid = NewScope("", ins.words[1].lword)
			scope.classscope = mtype
			scope.classname = modulename
			scope.privateclass = true
			scope.with = 'self'
			-- print(Serialize("ins",ins))
			-- print(mtype,#ins.words,#ins.words>1) if #ins.words>1 then print(ins.words[2].lword,ins.words[2].lword=="class",">>>>",#ins.words>1 and ins.words[2].lword=="class") end
			-- print(Serialize("module",scope))
			--ret = ret .."do "..script.."\t --[[ Module: "..modulename.."]]\t" -- Security measure to create a 'do' scope... Overall it never hurts, and some 'damage' can be prevented on the way.
			ret = ret .."do \t --[[ Module: "..modulename.."]]\t"
			ret = ret .. _Neil.Neil..".Class.Create(\""..modulename.."\", true)"
			-- print(ret)
		elseif (DeclaHelp(ins.words[1].lword) or IsType(ins.words[1].word)) and ins.kind~="pure" then
			local success,err = Declaration(ins,scope,alwaysplua,pluaprefix,localplua)
			if not success then return nil,err.." in line "..ins.linenumber.." ("..chunk..")" end
			scope,scopeid = CurrentScope() -- Since functions can be define here, this is important!
			ret = ret .. success
		elseif ins.words[1].lword == "defer" then
			local para,err
			if #ins.words<2 then return nil,"Incomplete defer ("..chunk..":"..ins.linenumber..")" end
			if ins.words[2].kind~="identifier" then return nil,"Unexpected "..ins.words[2].kind.." ("..ins.words[2].word..") in defer ("..chunk..":"..ins.linenumber..")" end
			if #ins.words>2 then
				if ins.words[3].word~="(" then return nil,"Invalid defer request ("..ins.words[2].word..") in defer ("..chunk..":"..ins.linenumber..")" end
				para,err = LitTrans(alwaysplua, pluaprefix, ins,4,nil,unknowns)				
				if ins.words[#ins.words].word~=")" then return nil,"Invalid defer request closure ("..ins.words[2].word..") in defer ("..chunk..":"..ins.linenumber..")" end
				para = Globals.TRIM.Value(para)
				para = Globals.LEFT.Value(para,#para-1)
			else
				para = ""
			end
			local deferid
			local i = #scopes 
			while (not Globals.SUFFIXED.Value(scopes[i].type,"function")) and scopes[i].type~="init" do 				
				if i<=0 or scopes[i].type=="script" then return nil,"Can't defer here ("..ins.words[2].word..") in defer ("..chunk..":"..ins.linenumber..")" end
				i = i - 1 
			end
			deferid = string.format("%s_defer",scopes[i].id)
			scopes[i].defer = true
			ret = ret .. deferid .. " = " .. deferid .. " or {};  "..deferid.."[#"..deferid.."+1] = { func="..GetIdentifier(ins.words[2].word,unknowns)..", param={"..para.."} }"
		elseif ins.words[1].lword=="quickmeta" then
			if scope.type~="script" then return nil,"I can't start a quickmeta on line "..ins.linenumber.." ("..chunk..")" end
			local qmtype,qmmodule,qmname,qmidentifier
			for i=2,#ins.words do
				local word = ins.words[i]
				if word.kind=="comment" then break end
				if qmidentifier then return nil,"QuickMeta Syntax Error in line "..ins.linenumber.." ("..chunk..")" end
				if word.lword == "class" or word.lword == "group" then
					if qmtype then return nil,"QuickMeta conflict in line "..ins.linenumber.." ("..chunk..")" end
					qmtype=word.lword
				elseif word.lword == "module" then
					if qmmodule then return nil,"QuickMeta dupe module in line "..ins.linenumber.." ("..chunk..")" end
					qmmodule = true
					local i=0
					repeat 
						i = i + 1
						qmname = scope.id .. "_module_"..i
					until not Globals.TABLECONTAINS.Value(modules,qmname)
				elseif word.kind == "identifier" then
				   qmidentifier = word.uword
				   if not qmmodule then					  
					  if Globals[qmidentifier] then return nil,"QuickMeta creates duplicate identifier in line "..ins.linenumber.." ("..chunk..")" end
				   end
				else
					return nil,"QuickMeta didn't exect "..word.kind.." "..word.word.." in line "..ins.linenumber.." ("..chunk..")"
				end
			end
			if (not qmmodule) and (not qmidentifier) then return nil,"QuickMeta nameless in line "..ins.linenumber.." ("..chunk..")" end
			qmtype = qmtype or "group"
			if not qmmodule then 
				qmname = _Neil.Neil..".Globals."..(qmidentifier:upper()) 
				Globals[qmidentifier:upper()] = { Type="QuickMeta_"..qmtype, QuickMetaType=qmtype, UndefinedConstant=true }
				-- error(Serialize("Globals",Globals)) -- Force debug
				-- for k,v in pairs(Globals) do print(k,v.Type) end error("OBSERVE!") -- forced debug
			end
			script,scope,scopeid = NewScope("","quickmeta")
			scope.QuickMetaData = {Type=qmtype, Module=qmmodule }
			ret = ret .. script
			if qmtype=="class" then
				ret = ret .. qmname .. " = function() return setmetatable({},{"
			else
				ret = ret .. "do "..qmname.." = setmetatable({},{"
			end
		elseif ins.words[1].lword=="if" or ins.words[1].lword=="while" then
			if (not allowground) and scope.type=="script" then return nil,ins.words[1].word.." statement not allowed in ground scope" end
			local trans,loc,err = NewConditionScope(ins.words[1].lword,ins,unknowns)
			if not trans then return nil,err.." in line "..ins.linenumber.." ("..chunk..")" end
			if ins.words[1].lword=="if" then
				ret = ret .. "if "..trans.." then "..loc.." "
			else
				ret = ret .. "while "..trans.." do "..loc.." "
			end
			scope,scopeid = CurrentScope()
		elseif ins.words[1].lword=="elseif" then
			if scope.type~="if" and scope.type~="elseif" then return nil,"'ElseIf without 'If' in line "..ins.linenumber.." ("..chunk..")" end
			_EndScope()
			local trans,loc,err = NewConditionScope(ins.words[1].lword,ins,unknowns)
			if not trans then return nil,err.." in line "..ins.linenumber.." ("..chunk..")" end
			ret = ret .. "elseif "..trans.." then "..loc.." "
			scope,scopeid = CurrentScope()
		elseif ins.words[1].lword=="else" then
			if scope.type~="if" and scope.type~="elseif" then return nil,"'Else without 'If' in line "..ins.linenumber.." ("..chunk..")" end
			if #ins.words>2 then
			   if ins.words[2].kind~="comment" then return nil,"Else does not take any more input" end
			end
			_EndScope()
			local locs = NewScope("","else")
			ret = ret .." else "..locs
			scope,scopeid = CurrentScope()
		elseif ins.words[1].lword=="return" then
			local ftype = GetFunctionType()
			local ud = ""
			if scope.type=="script" then
				for _,w in pairs(ins.words) do
					if w.kind=="comment" then return nil,"No comments allowed in returns in the ground scope ("..chunk..":"..ins.linenumber..")" end
				end	
				local a,e = LitTrans(alwaysplua, pluaprefix,ins,2,nil,unknowns)
				if not a then return nil,"Ground scope return error: "..e.." in line #"..ins.linenumber.." ("..chunk..")" end
				modules[#modules+1]=a
				ret = ret .. "-- delayed to end: "
			else
				local err
				ud,err = UnDefer()
				if not ud then return nil,(err or "No data from Undefer received (internal error. Please report) in line ")..ins.linenumber.." ("..chunk..")" end
				local fscope
				for i=#scopes,1,-1 do
					if Globals.SUFFIXED.Value(scopes[i].type,"function") or scopes[i].type=="init" or scopes[i].type=="script" then fscope = scopes[i]; break end
				end
				assert(fscope,"INTERNAL ERROR! Function scope for return could not be retrieved")
				if ftype=="void" then
					if #ins.words>2 then
						print(Serialize("ins",ins))
					   if ins.words[2].kind~="comment" then return nil,"Void functions may not return any value in line "..ins.linenumber.." ("..chunk..")" end
					end
					ret = ret .. ud.."\treturn;"
					fscope.returned = true
					if scope.type=="case" then scope.casereturned=true end
				elseif ftype=="plua" or ftype=="var" then
					local expression,error = LitTrans(alwaysplua, pluaprefix,ins,2,nil,unknowns)
					if not expression then return nil,error.." in line "..ins.linenumber.." ("..chunk..")" end
					ret = ret .. ud .."\treturn "..expression
					fscope.returned = true
					if scope.type=="case" then scope.casereturned=true end
				else
					local expression,error = LitTrans(alwaysplua, pluaprefix,ins,2,nil,unknowns)
					if not expression then return nil,error.." in line "..ins.linenumber.." ("..chunk..")" end
					ret = ret .. "return ".._Neil.Neil..".Globals.ConvType("..expression..",'"..ftype.."','return value',false)"
					fscope.returned = true					
					if scope.type=="case" then scope.casereturned=true end
				end
			end
		elseif ins.words[1].lword=="init" then
			local script
			if scope.type~="script" then return nil,"init only allowed in ground scope" end
			ret = ret .. " __neil_init_functions[#__neil_init_functions+1] = function()"
			script,scope,scopeid = NewScope("","init")
			scope.returntype="void"
			scope.startline = ins.linenumber
			-- scope,scopeid = CurrentScope();		
			ret = ret .. script
		elseif ins.words[1].lword=="try" then
			if #ins.words>1 and ins.word[2].kind~="comment" then return nil,"Try needs no parameters in line "..ins.linenumber.." ("..chunk..")" end
			if nocatch then 
				ret = ret .. "do"
			else
				ret = ret .. "local __neil_trycatch_success,__neil_trycatch_exception = xpcall(function() --[[try]]\t"
			end
			local script
			script,scope,scopeid = NewScope("","try")
			scope.tryline = ins.linenumber
			ret = ret .." "..script.."\t"
		elseif ins.words[1].lword=="catch" and ins.words[1].kind=="keyword" then
			local exceptionid,script
			if scope.type~="try" then return nil,"Catch without Try in line "..ins.linenumber.." ("..chunk..")" end
			if #ins.words>1 and ins.words[2].kind=="identifier" then 
				exceptionid=ins.words[2].uword  
			elseif #ins.words>1 and ins.words[#ins.words].kind~="comment" then 				
				return nil,"Catch syntax error in line "..ins.linenumber.." ("..chunk..")" 
			end
			if nocath then
				ret = ret .. "end if false then --[[ Catch ]] "
			else
				ret = ret .. "end, ".._Neil.Neil..".Catcher) if not __neil_trycatch_success then --[[ Catch ]] "
			end
			_EndScope()
			script,scope,scopeid = NewScope("","catch")
			if exceptionid then scope.identifiers[exceptionid] = "__neil_trycatch_exception" end
			ret = ret .." "..script.."\t"
			-- print(ret) -- HUH?
		elseif ins.words[1].lword=="finally" then
			return nil,"There is no 'finally' support yet in Neil. The word has been reserved for possible future possibilities! ("..chunk..":"..ins.linenumber..")"
		elseif ins.words[1].lword=="end" and ins.words[1].kind=="keyword" then
			-- print("End of scope: "..scopeid.." >> "..scope.type.." "..ins.linenumber) -- debug
			if suffixed(scope.type,"function") or suffixed(scope.type,"method") or scope.type=="init" then				
				if not scope.returned then
					local ud,err = UnDefer()
					if not ud then return nil,err .. " in line "..ins.linenumber.." ("..chunk..")" end
					ret = ret .. ud .."\t\t"
				end
				if (scope.returntype~="void" and scope.returntype~="var") and (not scope.returned) then
					ret = ret .. "return ".._Neil.Neil..".Globals.ConvType(nil,'"..scope.returntype.."','auto-return value',false)\t"
				end
				ret = ret .."end\t"
				if scope.closure then ret = ret..scope.closure.."\t" end
			elseif scope.type=="try" then
				return nil,"Try("..scope.tryline..") without Catch in line "..ins.linenumber.." ("..chunk..")"
			elseif scope.type=="repeat" then
				return nil,"Repeat scope cannot be ended with the End keyword (Line #"..ins.linenumber.." in chunk "..chunk..")"
			elseif scope.type=="case" or scope.type=="default" then
				ret = switches[switchmethod].replacer(ret,scopes[#scopes-1])
				local r,e = switches[switchmethod].einde(scopes[#scopes-1]) 				
				if not r then return nil,e," in line "..ins.linenumber.." ("..chunk..")" end
				ret = ret .. r
				scope,scopeid = CurrentScope()
			else
				-- print("Ending: "..scope.type..": "..scope.id.." in line #"..ins.linenumber)
				-- In all situations not noted above
				ret = ret .."end"				
			end
			ret = ret .. " --[[endscope: "..scope.id.."; "..scope.type.." ]] "			
			_EndScope()
			scope,scopeid = CurrentScope()
			if scope.type=="defmeta" or scope.type=="deftable" then ret = ret .. "," end
			-- print("End: ",#scopes,scope,scopeid,"Line: "..ins.linenumber)
		elseif ins.words[1].lword=="do" and ins.words[1].kind=="keyword" then
			script,scope,scopeid = NewScope("","do")
			ret = ret .. "do".." "..script
		elseif ins.words[1].lword=="for" then
			if (not allowground) and scope.type=="script" then return nil,ins.words[1].word.." statement not allowed in ground scope  ("..chunk..", line #"..ins.linenumber..")" end
			local its = {}
			local i = 1
			repeat
				i = i + 1 -- This skips 1, but 1 is only the keyword 'for' anyway
				if i>=#ins.words then return nil,"incomplete for-loop ("..chunk..", line #"..ins.linenumber..")" end
				if ins.words[i].word=="=" or ins.words[i].lword=="in" then 
				   -- print ("break",ins.words[i].lword=="in",ins.words[i].lword) 
				   break 
				elseif ins.words[i].kind == "identifier" then its[#its+1] = ins.words[i].word 
				elseif ins.words[i].word == "," then -- Nothing
				else return nil,"Unexpected "..ins.words[i].kind.." ("..ins.words[i].word..") in for-loop syntax ("..chunk..", line #"..ins.linenumber..")" end
			until false
			local s,e = LitTrans(alwaysplua, pluaprefix, ins,i+1,nil,unknowns)
			if not s then return s,e.."("..chunk..", line #"..ins.linenumber..")" end
			ret = ret .."for "
			for i,it in ipairs(its) do
				if i>1 then ret = ret ..", " end
				ret = ret .. it
			end
			-- print(i,ins.words[i].lword)
			ret =  ret .. " ".. ins.words[i].lword .." ".. s .." do "
			script,scope,scopeid = NewScope("","for-loop")
			ret = ret .. script
			for _,it in ipairs(its) do scope.identifiers[it:upper()] = it end
		elseif ins.words[1].lword=="import" then
			if #ins.words<=1 or ins.words[2].kind=="comment" then return nil,"Import what? in line #"..ins.linenumber.." ("..chunk..")" end
			if ins.words[3] and ins.words[3].kind=="comment" then ins.words[3] = nil end
			if ins.words[4] and ins.words[3].kind=="comment" then ins.words[4] = nil end
			local iword,xword = ins.words[2],ins.words[3] or ins.words[2]
			if ins.words[2].lword == "global" then
				iword,xword = ins.words[3],ins.words[4] or ins.words[3]
				if iword.kind~="identifier" then return nil,"Global identifier to import to expected in line #"..ins.linenumber.." ("..chunk..")" end
				if not iword then return nil,"Import global what? in line #"..ins.linenumber.." ("..chunk..")" end				
				if Globals[iword.uword] then return nil,"Import Global tries to create a duplicate identifier in line #"..ins.linenumber.." ("..chunk..")" end
				if keywords[iword.lword] then return nil,'"'..iword.word..'" is a keyword in line #'..ins.linenumber.." ("..chunk..")" end
				Globals[iword.uword] = {Type="plua", PLua=xword.word }
			else
				if iword.kind~="identifier" then return nil,"Identifier to import to expected in line #"..ins.linenumber.." ("..chunk..")" end
				if scope.identifiers[iword.uword] then return nil,"Import tries to create a duplicate identifier in line #"..ins.linenumber.." ("..chunk..")" end
				scope.identifiers[iword.uword] = xword.word
			end
			ret = ret .. "--[[ import request here ]] "
		elseif ins.words[1].lword=="export" then
			if #ins.words<=1 or ins.words[2].kind=="comment" then return nil,"Emport what? in line #"..ins.linenumber.." ("..chunk..")" end
			if ins.words[3] and ins.words[3].kind=="comment" then ins.words[3] = nil end
			local iword,xword = ins.words[2],ins.words[3] or ins.words[2]
			if iword.kind~="identifier" then return nil,"Identifier to export expected in line #"..ins.linenumber.." ("..chunk..")" end
			local id = GetIdentifier(iword.word,unknowns) 			
			ret = ret .. xword.word .. " = "..id.." --[[ EXPORT "..id.." ]]\t"
		elseif ins.words[1].lword=="repeat" then
			script,scope,scopeid = NewScope("","repeat")
			ret = ret .. "repeat\t"..script
		elseif ins.words[1].lword=="until" then
			if scope.type~="repeat" then return nil,"Until without Repeat in line #"..ins.linenumber.." ("..chunk..")" end
			local scr,err = LitTrans(alwaysplua, pluaprefix, ins,2,nil,unknowns)
			if not scr then return s,err.."("..chunk..", line #"..ins.linenumber..")" end
			ret = ret .. "until "..scr.."\t"
			_EndScope()
			scope,scopeid = CurrentScope()
		elseif ins.words[1].lword=="loopwhile" then
			if scope.type~="repeat" then return nil,"LoopWhile without Repeat in line #"..ins.linenumber.." ("..chunk..")" end
			local scr,err = LitTrans(alwaysplua, pluaprefix,ins,2,nil,unknowns)
			if not scr then return s,err.."("..chunk..", line #"..ins.linenumber..")" end
			ret = ret .. "until not("..scr..")\t"
			_EndScope()
			scope,scopeid = CurrentScope()
		elseif ins.words[1].lword=="forever" then
			if scope.type~="repeat" then return nil,"Forever without Repeat in line #"..ins.linenumber.." ("..chunk..")" end
			ret = ret .. "until false\t"
			_EndScope()
			scope,scopeid = CurrentScope()
		elseif ins.kind=="decrement" then
			local s,e = LitTrans(alwaysplua, pluaprefix,ins,1,nil,unknowns)
			if not s then return s,e.."("..chunk..", line #"..ins.linenumber..")" end
			ret = ret .. s .. " = " .. _Neil.Neil..".Dec(" ..s..") "
		elseif ins.kind=="increment" then
			local s,e = LitTrans(alwaysplua, pluaprefix,ins,1,nil,unknowns)
			if not s then return s,e.."("..chunk..", line #"..ins.linenumber..")" end
			ret = ret .. s .. " = " .. _Neil.Neil..".Inc(" ..s..") "
		elseif ins.kind=="add" then
			local ains = { words = ins.define }
			local dw,de = LitTrans(alwaysplua, pluaprefix, ains,1,nil,unknowns); if not dw then return nil,de.."("..chunk..", line #"..ins.linenumber..")" end
			local vw,ve = LitTrans(alwaysplua, pluaprefix, ins,1,nil,unknowns); if not vw then return nil,ve.."("..chunk..", line #"..ins.linenumber..")" end
			ret = ret .. dw .. " = ".. _Neil.Neil..".Add("..dw..", "..vw..")"
		elseif ins.kind=="subtract" then
			local ains = { words = ins.define }
			local dw,de = LitTrans(alwaysplua, pluaprefix, ains,1,nil,unknowns); if not dw then return nil,de.."("..chunk..", line #"..ins.linenumber..")" end
			local vw,ve = LitTrans(alwaysplua, pluaprefix, ins,1,nil,unknowns); if not vw then return nil,ve.."("..chunk..", line #"..ins.linenumber..")" end
			ret = ret .. dw .. " = ".. _Neil.Neil..".Subtract("..dw..", "..vw..")"
		elseif ins.words[1].word=="with" then
			local script
			script,scope,scopeid = NewScope("","with")
			ret = ret .."do "..script
			local dw,de = LitTrans(alwaysplua, pluaprefix,ins,2,unknowns); if not dw then return nil,de.."("..chunk..", line #"..ins.linenumber..")" end
			ret = ret .."\t local with_"..scopeid.." = "..dw
			scope.with = "with_"..scopeid
			print("with "..scope.with.." <= "..dw)
		elseif ins.words[1].word=="break" then
			ret = ret .. "break\t"
		elseif (ins.words[1].kind=="identifier" or ins.words[1].kind=="with-mark" or ins.words[1].kind=="operator") and ins.kind=="instruction" then
		   if (not allowground) and scope.type=="script" then 
				-- print(Serialize("Forbidden instruction",ins))
				return nil,"Instruction not allowed in ground scope ("..chunk..", line #"..ins.linenumber..")" 
		   end
		   local result,error = LitTrans(alwaysplua or localplua, pluaprefix,ins,1,nil,unknowns,ins)
		   if not result then return nil,error.." in line "..ins.linenumber.." ("..chunk..")" end
		   ret = ret .. result
		  --  error("Identifier from start processing not yet implemented")
		elseif ins.words[1].lword=="deftable" or ins.words[1].lword=="defmeta" then
			local ttype = ins.words[1].lword
			local idn = 2
			if #ins.words<1 then return nil,"'deftable/defmeta' expects more input in line "..ins.linenumber.." ("..chunk..")" end
			if ins.words[2].kind=="keyword" then
				if ins.words[2].lword~="plua" and ins.words[2].lword~="table" and ins.words[2].lword~="var" then
					return nil,"Type of either TABLE or VAR or PLUA expected in line "..ins.linenumber.." ("..chunk..")" 
				end
				idn = 3
				return nil,"no support yet for deftable and declare in once in line "..ins.linenumber.." ("..chunk..")" 
			end
			if #ins.words<idn then return nil,"'deftable/defmeta' expects more input in line "..ins.linenumber.." ("..chunk..")" end
			if ins.words[idn].kind ~= "identifier" then
				print(idn)
				return nil,"Identifier expected in line "..ins.linenumber.." ("..chunk..")"
			end
			local id = ins.words[idn].word
			local wid = GetIdentifier(id,unknowns,unknownprefix,ins)
			ret = ret .. "do\t"..wid.." = "
			if ttype=="defmeta" then
				ret = ret .. "setmetatable({},{" 
			else
				ret = ret .. "{"
			end
			local script
			script,scope,scopeid = NewScope("",ttype)
			scope.wid = wid
		else
			-- print("<translation>\n","\r"..ret.."\n</translation>") -- debug
			print(Serialize("Instruction_"..insid,ins))
			error("I do not yet understand instruction "..insid.." in line "..ins.linenumber.." of chunk "..chunk..".\n I'm still being developed after all")
		end
	end
	if scope.type~="script" then 
		if scope.startline then
			return nil,"The "..scope.type.." scope in line "..scope.startline.." has not been properly ended  ("..chunk..")"
		else
			return nil,scope.type.." not properly ended ("..chunk..")"
		end
	end
	_EndScope() -- Script scope must end too, you know!
	ret = ret .."\n\n--[[ Closure ]] for _,initfunc in ipairs(__neil_init_functions) do initfunc() end; __neil_init_functions = nil\n"
	if #modules>0 then
		for i,v in ipairs(modules) do
			if i==1 then ret = ret .."return " else ret = ret ..", " end
			ret = ret..v
		end
	end
	if showtranslation then print("<translation Chunk=\""..chunk.."\">\n","\r"..ret.."\n</translation>") end -- debug
	do
		local unk=""
		local uid=0
		unknowns.satisfied = unknowns.satisfied or {}
		for i,u in Globals.SPAIRS.Value(unknowns.unknown or {}) do
			if unknowns.satisfied[u.id] then
				ret = MacroReplace(ret,"@"..u.id.."@",GetIdentifier(u.id,nil,{}))
			else				
				uid = uid + 1
				unk = unk .. "= "..u.id.." in line #"..u.linenumber.."("..tostring(chunk)..")\n"
			end
		end
		-- print("Check: "..uid.."\n"..Serialize("u",unknowns))
		if uid==1 then
		   unk = "There was an unknown identifier found:\n"..unk
		   return nil,unk
		elseif uid>0 then
		   unk = "There were "..uid.." unknown identifiers found:\n"..unk
		   return nil,unk
		end
	end	    
	--print(ret)
	return ret,"Ok"
end

function _Neil.Translate(script,chunk)
	TranslationCount = TranslationCount + 1
	-- print("Ori\n",script,"/Ori")
	local macroed,macroerror = Macro(script,chunk); if not macroed then _Neil.Error("Macro error: "..macroerror) return nil,"Macro error: "..macroerror end
	-- print(macroed) error("Crash!") -- debug
	local chopped = Chop(macroed.."\n\n",chunk)
	-- print(Serialize("chopped",chopped))
	-- return nil,"Sorry translate does not yet work!"
	local ret,error = Translate(chopped,chunk)
	if not ret then 
		return nil,"Neil Translation error: "..error 
	end
	-- print("<translation>\n"..ret.."\n</translation>") -- debug only!!!
	return ret,"Ok"
end


-- Load
function _Neil.Load(script,chunk)
	local script,err = _Neil.Translate(script,chunk)	
	_Neil.Assert(script,"Translation error:\n"..err)
	if not script then return end -- Safety precaution!
	local ret,err = (loadstring or load)(script,chunk)
	if not ret then print(script) end
	_Neil.Assert(ret,err)
	return ret
end

function _Neil.Use(module,chunk)
	local key,err
	local used
	if usedebug then print("#use","using",module,chunk) end
	-- chunk = chunk or module
	if _Neil.FileSystemCaseSensitive then
		key = module
	else
		key = module:lower()
	end	
	if UsedByNeil[key] then
		return UsedByNeil[key]
	end
	if NeilIsUsing[key] then error("Cyclic use request: "..module) end
	NeilIsUsing[key] = true
	-- print(_Neil.FileExists,fileExists	)
	_Neil.ReadFile   = _Neil.ReadFile   or readAll
	_Neil.ReadDir    = _Neil.ReadDir    or readDir
	_Neil.FileExists = _Neil.FileExists or fileExists
	_Neil.DirExists  = _Neil.DirExists  or dirExists
	if usedebug then print("#use","Scanning for existence") end
	if     _Neil.FileExists(module..".neil") then used,err =  _Neil.UseFile(module..".neil",chunk) 
	elseif _Neil.DirExists(module..".neilbundle") then used,err = _Neil.UseDir(module..".neilbundle",chunk) 
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
	if not _Neil.Assert(used,"Use failure\n"..(err or "No importable module name \""..module.."\" found!")) then return nil,err end	
	NeilIsUsing[key] = nil
	UsedByNeil[key] = used
	return used
end


-- Warning! Best is to NEVER call this function directly unless you know what you are doing!
function _Neil.UseFile(file,chunk)	
	local s,e = _Neil.ReadFile(file)
	if not s then return nil,e end
	local tr,er = _Neil.Translate(s,chunk or file)
	if not tr then return nil,er end
	local comp
	comp,e = (loadstring or load)(tr,chunk or file)
	if not comp then 
		print(tr)
		return nil,e 
	end
	-- print(tr)
	return comp() or true
end

-- Warning! Best is to NEVER call this function directly unless you know what you are doing!
function _Neil.UseDir(dir,chunk)
	local d = _Neil.ReadDir(dir)
	local s = ""
	if _Neil.FileExists(dir.."/_neilbundle.neil") then return _Neil.UseFile(dir.."/_neilbundle.neil") end
	for _,f in ipairs(d) do
		if _Neil.Globals.Right(f,5):lower()==".neil" then
			s = s .. "#use \"".._Neil.Globals.Left(f,#f-5).."\"\n"
		end
	end
	print("#usebundle","\n"..s,"</>")
	local comp,err = _Neil.Load(s,"NEILBUNDLE: "..dir)
	if not comp then return nil,e end
	return comp() or true
end


	
-- Closure
ExceptLoad() -- Must be last in order to make sure the translator is fully compiled now!

function _Neil.DumpGlobals() -- FOR DEBUG PURPOSES ONLY!!!
	for k,v in Globals.SPAIRS.Value(Globals) do
		if k=="LUA" then
		   print("LUA = _G -- Hands off!");
		else
			print(Serialize("Global "..k,v))
		end
	end
end


-- return _Neil
return setmetatable({},{
	__index = function(s,k)
		-- print("Get",k)
		if not _Neil[k] then
			error("Neil does not have a member named "..k)
			-- Assert would have to create the string above regardless if the condition was right or not!
			-- That would only cause a needless slowdown due to RAM allocations and release that are never needed until things actually do go wrong!
			-- And when things do go wrong that extra action doesn't matter anymore.
		end
		return _Neil[k]
	end,
	__newindex = function(s,k,v)
		-- print("Modify",k,"for",v)
		if k=="Neil" then
			if TranslationCount==1 then
				error("There's already a translation done! Therefore the field 'Neil' cannot be modified anymore!")
			elseif TranslationCount>1 then
				error(TranslationCount.." translations have been done, already! Therefore the field 'Neil' cannot be modified anymore!")
			else
				assert(type(v)=="string","Field Neil must contain a string value and not a "..type(v))
				_Neil.Neil = v
			end
		elseif k=="ReadFile" then
			_Neil.ReadFile   = v or readAll		assert(type(_Neil.ReadFile)=="function","Field ReadFile must be a function, and not a "..type(v))
		elseif k=="ReadDir" then
			_Neil.ReadDir    = v or readDir		assert(type(_Neil.ReadDir)=="function","Field ReadDir must be a function, and not a "..type(v))
		elseif k=="FileExists" then
			_Neil.FileExists = v or fileExists  assert(type(_Neil.FileExists)=="function","Field FileExists must be a function, and not a "..type(v))
		elseif k=="DirExists" then
			_Neil.DirExists  = v or dirExists	assert(type(_Neil.DirExists)=="function","Field DirExists must be a function, and not a "..type(v))
		else
			error("Neil field "..k.." is read-only")
		end
	end
})