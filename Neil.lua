-- Creation of library
local _Neil = {}
_Neil.Neil = "Neil"

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
	if not condition then _Neil.Error(err) end
end


-- Globals
local Globals
Globals = {
	['LUA'] = { Type='table', Value=_G, Constant=true },
	['GLOBALDUMP'] = { Type='delegate', Constant=true, Value=function() local ret="" for k,v in pairs(Globals) do ret = ret .. k .. " = "..tostring(v) end end },
	['SOUT'] = {Type='delegate', Constant=true,Value=function(...) 
			local ret = ""
			for _,v in ipairs{...} do ret = ret .. Globals.TOSTRING.Value(v) end
			return ret
		end},
	['COUT'] = {Type='delegate', Constant=true,Value=function(...) io.write(Globals.SOUT.Value(...)) end },
	['TOSTRING'] = {Type='delegate', Constant=true,Value=function(v) return ConvType(v,"string") end },
	["REPLACE"] = {Type='delegate', Constant=true,Value=string.gsub },
	['TRIM'] = {Type='delegate', Constant=true,Value=function(str) return (Neil.Globals.ToString(s):gsub("^%s*(.-)%s*$", "%1")) end }
	
}
_Neil.Globals = setmetatable({},{
     __index = function(s,k)
          local uk = k:upper()
          -- Chat(uk,tostring(Globals[uk]))
          _Neil.Assert(Globals[uk],"Reading unknown global identifier \""..k.."\"!")
          -- print(uk) for k,v in pairs(Globals[uk]) do print(k,v) end -- debug line --
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

-- Scopes
local Scopes = { [0] = {ID="NOTHINGNESS"} }
local Locals = { NOTHINGNESS = {} }

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


-- Closure
return _Neil
