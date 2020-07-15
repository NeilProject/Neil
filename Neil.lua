-- Creation of library
local _Neil = {}
_Neil.Neil = "Neil"

-- Constant/ReadOnly
local ReadOnlyWrite = {
   ['LUA'] = { Type='table', Value=_G, Constant=true }
}

-- Check Type
local CTCase
local function ConvType(v,wanttype,key)  
  CTCase = CTCase or {
      local kk = ""
      if key then
         kk = " for "..key
      end
      byte = function(v)
         
         _Neil.Assert(type(v)=="number","Number required"..kk)
         v = math.floor(v+.5)
         if v<0 then
            return 255-(math.abs(v)%255)
         else
            return v%255
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
       ['boolean'] = function(v) return CTCase[wanttype].bool(v) end
       ['table'] = function(v)
          _Neil.Assert(type(v)=="table","Table required"..kk)
          return v
       ['userdata'] = function(v)
          _Neil.Assert(type(v)=="userdata","UserData required"..kk)
          return v
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
  
  return CTCase[wanttype](v)
end

-- Globals
local Globals = {}
_Neil.Globals = setmetatable({},{
     __index = function(s,k)
          local uk = k:upper()
          _Neil.Assert(Globals[uk],"Unknown global identifier \"..k..\"!")
          return Globals[uk].Value
        end,
     __newindex = function(s,k,v)
          local uk = k:upper()
          local want = Globals[uk]
          _Neil.Assert(want,"Unknown global identifier \"..k..\"!")          
          _Neil.Assert(not want.Constant,k.." is a constant and cannot be overwritten")
          _Neil.Assert((not want.ReadOnly) or (ReadOnlyWrite.Globals),k.." is read-only and cannot be overwritten")
          Globals[uk].Value = ConvType(v,Globals[uk].Type,k)
        end,
      __call(s,newk,oftype,rw,defaultvalue)
          local uk = newk:upper()
          _Neil.Assert(not Globals[uk],"Duplicate global identifier "..newk)
          local newdec = {}
          if oftype=="string"
              defaultvalue = defaultvalue or ""
          elseif oftype=="number" or oftype=="byte" or oftype=="int"
              defaultvalue = defaultvalue or 0
          elseif oftype=="boolean" or oftype=="bool"
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



-- Closure
return _Neil
