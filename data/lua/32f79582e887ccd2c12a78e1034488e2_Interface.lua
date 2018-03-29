require("lmake.GlobalVars").forbid()

local loadfile = loadfile
local tostring = tostring

local format   = string.format
local replace  = string.gsub
local find     = string.find
local concat   = table.concat
local insert   = table.insert
local shell    = os.execute
local append   =    function (array, element)
                        insert(array, element)
                    end

local error      = require("lmake.error")
local throwError = error.throwError
local pcall      = error.pcall
local ecall      = error.ecall
local epcall     = error.epcall
local INTERNAL   = error.INTERNAL
local EXTERNAL   = error.EXTERNAL

local system                  = require("lmake.system")
local newUserData             = system.newUserData
local setCurrentDirectory     = system.setCurrentDirectory

local core = require("lmake.core")

local file                    = require("lmake.file")
local getFileMtime            = file.getFileMtime
local getFileDirName          = file.getFileDirName
local getFileBaseName         = file.getFileBaseName
local getNormalizedFileName   = file.getNormalizedFileName
local getAbsoluteFileName     = file.getAbsoluteFileName
local isAbsoluteFileName      = file.isAbsoluteFileName
local getShortFileName        = file.getShortFileName
local getRelativeFileName     = file.getRelativeFileName
local initialWorkingDirectory = file.initialWorkingDirectory


local ObjectContainer       = require("lmake.ObjectContainer")
local OnTheFlyObjects       = require("lmake.OnTheFlyObjects")
local ListInterface         = require("lmake.ListInterface")
local OrderedSet            = require("lmake.OrderedSet")


local ActionExecutor        = require("lmake.ActionExecutor")
local executeAction         = ActionExecutor.executeAction



local function IndexIterator()
    local i = 0
    return function () 
        i = i + 1 
        return i 
    end
end

local interfaces   = {} -- maps absolute referingMakefileName to interface object

local phonyObjects  = {} -- maps absolute file names to internal phony objects
local isPhonyObject = {} -- maps internal phony objects to true

local function getPhonyObject(fileName)
    local rslt = phonyObjects[fileName]
    if not rslt then
        rslt = { name = fileName }
        phonyObjects[fileName] = rslt
        isPhonyObject[rslt] = true
    end
    return rslt
end

local function fileToString(fileNameObject)
    if type(fileNameObject) == "string" then
        return getShortFileName(initialWorkingDirectory, fileNameObject)
    elseif isPhonyObject[fileNameObject] then
        return getShortFileName(initialWorkingDirectory, fileNameObject.name)
    else
        throwError(INTERNAL, "invalid file name object '%s'", tostring(fileNameObject))
    end
end
core          .setFileToStringFunction(fileToString)
ActionExecutor.setFileToStringFunction(fileToString)

local interfaceToPrivateData = {}

local includingFilesStack = {}
local includingFiles      = {}

local function Interface(interfaceMakefileName, globals)

    local globals = globals or getfenv(2)

    local interfaceWorkingDirectory
    local interfaceMakefileAbsoluteName
    
    if interfaceMakefileName then
        interfaceWorkingDirectory     = getFileDirName(interfaceMakefileName)
        interfaceMakefileAbsoluteName = getAbsoluteFileName(interfaceMakefileName)
        interfaceMakefileName         = getFileBaseName(interfaceMakefileName)
    else
        interfaceWorkingDirectory     = initialWorkingDirectory
        interfaceMakefileAbsoluteName = nil
    end
    local currentFileAbsoluteName     = interfaceMakefileAbsoluteName

    local interface   = {}
    local privateData = {}
    
    interface.thisFile = interfaceMakefileName
    interface.thisDir  = interfaceMakefileName and getFileDirName(interfaceMakefileName)

    if interfaceMakefileAbsoluteName then
        local existingInterface = interfaces[interfaceMakefileAbsoluteName]
        if existingInterface then
            return existingInterface
        end
        interfaces[interfaceMakefileAbsoluteName] = interface
    end
    
    interfaceToPrivateData[interface]  = privateData

    local myFileNames                    = {} -- maps to the form they are used in this makefile
    local thisSubprojectFiles            = OrderedSet()
    local thisSubprojectAbsoluteFiles    = OrderedSet()
    local thisAllSubprojectAbsoluteFiles = OrderedSet()
    local thisParentInterfaces           = OrderedSet()

    
    local function reportSubprojectFileToParentMakefiles(fileName)
        thisAllSubprojectAbsoluteFiles:addElement(fileName)
        for p in thisParentInterfaces() do
            interfaceToPrivateData[p].reportSubprojectFile(fileName)
        end
    end

    privateData.fileNames                  = myFileNames
    privateData.subprojectFiles            = thisSubprojectFiles
    privateData.subprojectAbsoluteFiles    = thisSubprojectAbsoluteFiles
    privateData.parentInterfaces           = thisParentInterfaces
    privateData.reportSubprojectFile       = reportSubprojectFileToParentMakefiles
    privateData.allSubprojectAbsoluteFiles = thisAllSubprojectAbsoluteFiles
    
    do
        local interfaceFunctionEnvironmentMetatable = 
        {
             __index    =    function (table, key)
                                 return interface[key] or globals[key]
                             end,
        }
        local interfaceFunctionEnvironment = {}
              interfaceFunctionEnvironment._G = interfaceFunctionEnvironment
        setmetatable(interfaceFunctionEnvironment, interfaceFunctionEnvironmentMetatable)
        privateData.functionEnvironment = interfaceFunctionEnvironment
    end


    local function handleFileNameFromMakefile(fileName)
        
        local absName = getAbsoluteFileName(interfaceWorkingDirectory, fileName)

        local existingMyFileName = myFileNames[absName]
        
        local isAbs = isAbsoluteFileName(fileName)
        
        if existingMyFileName and (isAbs ~= isAbsoluteFileName(existingMyFileName)) 
        then
            throwError(EXTERNAL, "File name is used in absolute and relative form: '%s' <-> '%s'",
                                 fileName, existingMyFileName)
        end
        
        if not existingMyFileName then
            if isAbs then
                existingMyFileName = absName
            else
                existingMyFileName = getRelativeFileName(interfaceWorkingDirectory, absName)
            end
            myFileNames[absName] = existingMyFileName
        end
        return absName, existingMyFileName
    end
    
    if interfaceMakefileAbsoluteName then
        -- own Makefile is always relativ name
        handleFileNameFromMakefile(getRelativeFileName(interfaceWorkingDirectory, interfaceMakefileAbsoluteName))
    end

    local function normalizeFileNameFromMakefile(fileName)
        local absName, existingMyFileName = handleFileNameFromMakefile(fileName)
        return existingMyFileName
    end

    local function absolutizeFileNameFromMakefile(fileName)
        local absName, existingMyFileName = handleFileNameFromMakefile(fileName)
        return absName
    end

    local function internalizeFileNameFromMakefile(fileName)
        local absName, existingMyFileName = handleFileNameFromMakefile(fileName)
        local dirName = getFileDirName(absName)
        if interfaces[dirName] then
            handleFileNameFromMakefile(getFileDirName(fileName))
            local interf = assert(interfaces[dirName], dirName)
            return getPhonyObject(absName)
        else
            return absName
        end
    end

    
    
    local function fileNameStringForMakefile(fileName)
        
        local phonyName
        
        if isPhonyObject[fileName] then
            phonyName = getFileBaseName(fileName.name)
            fileName  = getFileDirName(fileName.name)
        end

        local function innerFunction()
            local absName = getAbsoluteFileName(interfaceWorkingDirectory, fileName)
    
            local existingMyFileName = myFileNames[absName]
            
            if existingMyFileName then
                return existingMyFileName
            end
            
            local function searchFileNameInSubprojectFiles(thisInterface)
                local thisPrivateData = interfaceToPrivateData[thisInterface]
                for i, incFile in thisPrivateData.subprojectFiles:ipairs() do
                    local absIncFile  = thisPrivateData.subprojectAbsoluteFiles[i]
                    local interfIsAbs = isAbsoluteFileName(incFile)
                    local interf      = interfaces[absIncFile]
                    local fileNames   = interfaceToPrivateData[interf].fileNames
                    local foundName   = fileNames[absName]
                    if foundName then
                        if interfIsAbs or isAbsoluteFileName(foundName) then
                            return absName
                        else
                            return getRelativeFileName(interfaceWorkingDirectory, absName)
                        end
                    else
                        return searchFileNameInSubprojectFiles(interf)
                    end
                end
            end
            local foundName = assert(searchFileNameInSubprojectFiles(interface), fileName)
            return foundName
        end
        local foundName = innerFunction()
        if phonyName then
            return foundName.."/"..phonyName
        else
            return foundName
        end
    end
    
    
    local function checkAndNormalizeFileName(arg, argName)
        argName = argName or "file name"
        if type(arg) == "string" then
            if #arg == 0 then
                throwError(EXTERNAL, "empty filename argument for parameter '%s'", argName)
            else
                return internalizeFileNameFromMakefile(arg)
            end
        else
            throwError(EXTERNAL, "filename argument for parameter '%s' must be a string", argName)
        end
    end
    local function checkAndNormalizeFileList(arg, argName)
        if type(arg) == "string" then
            return { internalizeFileNameFromMakefile(arg) }
        else
            if type(arg) == "function" then
                local newArg = {} 
                repeat
                    local e = arg()
                    append(newArg, e)
                until not e
                arg = newArg           
            end
            if type(arg) ~= "table" then
                throwError(EXTERNAL, "filename argument for parameter '%s' must be a string or a list of strings", argName)
            end
            local rslt = {}
            for _, a in ipairs(arg) do
                if type(a) ~= "string" then
                    throwError(EXTERNAL, "filename in  parameter list '%s' must be a string", argName)
                elseif #a == 0 then
                    throwError(EXTERNAL, "empty filename in  parameter list '%s'", argName)
                end
                append(rslt, internalizeFileNameFromMakefile(a))
            end
            return rslt
        end
    end
    
    local thisTargets      = OrderedSet()
    local thisPhonyTargets = OrderedSet()
    
    local checkAndNormalizeTargetName,
          checkAndNormalizeTargetList
    do
        local function checkTarget(nameObject)
            if type(nameObject) == "string" then
                thisTargets:addElement(assert(myFileNames[nameObject], tostring(nameObject)))
            else
                thisTargets:addElement     (assert(myFileNames[nameObject.name], tostring(nameObject)))
                thisPhonyTargets:addElement(assert(myFileNames[nameObject.name], tostring(nameObject)))
            end
            return nameObject
        end
        function checkAndNormalizeTargetName(arg, argName)
            return checkTarget(checkAndNormalizeFileName(arg, argName))
        end
        function checkAndNormalizeTargetList(arg, argName)
            local list = checkAndNormalizeFileList(arg, argName)
            for _, name in ipairs(list) do
                checkTarget(name)
            end
            return list
        end
    end
    
    
    
    interface.subprojects  = ListInterface(thisSubprojectFiles)
    
    interface.targets      = ListInterface(thisTargets)
    
    interface.phonyTargets = ListInterface(thisPhonyTargets)
    
    interface.isPhony      =            function (name)
                                            return isPhonyObject[internalizeFileNameFromMakefile(name)] == true
                                        end
    
    interface.phony = OnTheFlyObjects(  function (arg1, arg2)
                                            if arg2 then
                                                local makefileNames = checkAndNormalizeFileList(arg1, "makefile")
                                                local rslt = {}
                                                if type(arg2) ~= "string" or #arg2 == 0 then
                                                    throwError(EXTERNAL, "phony name argument must be a string")
                                                end
                                                for _, absName in ipairs(makefileNames) do
                                                    local otherInterface = interfaces[absName]
                                                    if not otherInterface then
                                                        throwError(EXTERNAL, "makefile '%s' is unknown", fileToString(absName))
                                                    end
                                                    local pseudoFileName = myFileNames[absName].."/"..arg2
                                                    handleFileNameFromMakefile(pseudoFileName)
                                                    append(rslt, pseudoFileName)
                                                end
                                                if #rslt == 1 and type(arg1) == "string" then
                                                    return nil, rslt[1]
                                                else
                                                                    local i = 0
                                                    return  nil,    function ()
                                                                        if i < #rslt then
                                                                            i = i + 1
                                                                            return rslt[i]
                                                                        end
                                                                    end
                                                end
                                            else
                                                if type(arg1) ~= "string" then
                                                    throwError(EXTERNAL, "Phony name must be a string value.")
                                                end
                                                if find(arg1, "/") then
                                                    throwError(EXTERNAL, "Phony name cannot contain '/' character.")
                                                end
                                                local pseudoFileName = interfaceMakefileName.."/"..arg1
                                                handleFileNameFromMakefile(pseudoFileName)
                                                return arg1, pseudoFileName
                                            end
                                        end )
    
    function interface.getCurrentDirectory()
        return interfaceWorkingDirectory
    end
    
    function interface.getFileMtime(fileName)
        if type(fileName) ~= "string" or fileName == "" then
            throwError(EXTERNAL, "Argument for function 'getFileMtime' must be none-empty file name string")
        end
        return ecall(INTERNAL, getFileMtime, absolutizeFileNameFromMakefile(fileName))
    end

    function interface.getFileDirName(fileName)
        if type(fileName) ~= "string" or fileName == "" then
            throwError(EXTERNAL, "Argument for function 'getFileDirName' must be none-empty file name string")
        end
        return ecall(INTERNAL, getFileDirName, fileName)
    end

    function interface.getAbsoluteFileName(fileName)
        if type(fileName) ~= "string" or fileName == "" then
            throwError(EXTERNAL, "Argument for function 'getAbsoluteFileName' must be none-empty file name string")
        end
        return ecall(INTERNAL, getAbsoluteFileName, interfaceWorkingDirectory, fileName)
    end

    function interface.getNormalizedFileName(fileName)
        if type(fileName) ~= "string" or fileName == "" then
            throwError(EXTERNAL, "Argument for function 'getNormalizedFileName' must be none-empty file name string")
        end
        return ecall(INTERNAL, normalizeFileNameFromMakefile, fileName)
    end
    
    function interface.subproject(fileName)

        if type(fileName) ~= "string" or fileName == "" then
            throwError(EXTERNAL, "Argument for function 'subproject' must be non-empty file name string")
        end
    
        local fileName = normalizeFileNameFromMakefile(fileName)
    
        local absoluteFileName = absolutizeFileNameFromMakefile(fileName)
        
        thisSubprojectFiles        :addElement(fileName)
        thisSubprojectAbsoluteFiles:addElement(absoluteFileName)

        reportSubprojectFileToParentMakefiles(absoluteFileName)

        if interfaces[absoluteFileName] then
            local existingInterface = interfaces[absoluteFileName]
            local existingPrivateData = interfaceToPrivateData[existingInterface]
            if      interfaceMakefileAbsoluteName 
                and existingPrivateData.allSubprojectAbsoluteFiles:hasElement(interfaceMakefileAbsoluteName)
            then
                throwError(EXTERNAL, "Cyclic makefile inclusion: '%s' is already including '%s'.", 
                                     fileToString(absoluteFileName), fileToString(interfaceMakefileAbsoluteName))
            end
            existingPrivateData.parentInterfaces:addElement(interface)
            return
        end
        
            local func, errorMessage = loadfile(fileToString(absoluteFileName))
    
            if func then
                local newInterface = Interface(absoluteFileName, globals)
                
                interfaceToPrivateData[newInterface].parentInterfaces:addElement(interface)

                setfenv(func, interfaceToPrivateData[newInterface].functionEnvironment)

                                                            includingFiles[absoluteFileName] = currentFileAbsoluteName or true
                local wasOK, rslt = epcall(EXTERNAL, func)
                                                            includingFiles[absoluteFileName] = nil
                if not wasOK then
                    throwError(rslt)
                end
            else
                throwError(EXTERNAL, errorMessage)
            end
    end
    
    
    function interface.include(fileName)

        if type(fileName) ~= "string" or fileName == "" then
            throwError(EXTERNAL, "Argument for function 'include' must be non-empty file name string")
        end
    
        local fileName = normalizeFileNameFromMakefile(fileName)
    
        local absoluteFileName = absolutizeFileNameFromMakefile(fileName)
        
        if includingFiles[absoluteFileName] then
            if type(includingFiles[absoluteFileName]) == "string" then
                throwError(EXTERNAL, "File '%s' is already included from '%s'", fileToString(absoluteFileName),
                                                                                fileToString(includingFiles[absoluteFileName]))
            else
                throwError(EXTERNAL, "File '%s' is already included from commandline", fileToString(absoluteFileName))
            end
        end

            local func, errorMessage = loadfile(fileToString(absoluteFileName))
    
            if func then
                setfenv(func, interfaceToPrivateData[interface].functionEnvironment)

                                                            local oldCurrentFileAbsoluteName = currentFileAbsoluteName
                                                            local oldCurrentFile             = interface.thisFile
                                                            includingFiles[absoluteFileName] = currentFileAbsoluteName
                                                            currentFileAbsoluteName          = absoluteFileName
                                                            interface.thisFile               = fileName
                                                            interface.thisDir                = getFileDirName(fileName)
                local wasOK, rslt = epcall(EXTERNAL, func)
                                                            includingFiles[absoluteFileName] = nil
                                                            interface.thisFile               = oldCurrentFile
                                                            interface.thisDir                = getFileDirName(oldCurrentFile)
                                                            currentFileAbsoluteName          = oldCurrentFileAbsoluteName
                if not wasOK then
                    throwError(rslt)
                end
            else
                throwError(EXTERNAL, errorMessage)
            end
        
    end

    function interface.Dep(...)
        local arg
        if select("#", ...) == 1 then
            arg = select(1, ...)
            if type(arg) ~= "table" then
                throwError(EXTERNAL, "argument for Dep must be one table or target and prereq")
            end
        else
            arg = { ... }
        end
        
        local next = IndexIterator()
        
        local target = arg.target or arg[next()]
        local prereq = arg.prereq or arg[next()]
        if not target then
            throwError(EXTERNAL, "missing target argument for Dep")
        end
        if not prereq then
            throwError(EXTERNAL, "missing prereq argument for Dep")
        end
        local targetFileName = checkAndNormalizeFileName(target, "target")
        local prereqFileName = checkAndNormalizeFileName(prereq, "prereq")
        if phonyObjects[targetFileName] then
            thisTargets:addElement(myFileNames[targetFileName])
        end
        ecall(INTERNAL, core.addDep, targetFileName, 
                                     prereqFileName)
    end
    
    local function fileInfoInFilter(fileName)
        core.getFileInfo(internalizeFileNameFromMakefile(fileName))
    end
    local function fileInfoOutFilter(fileInfo)
        return fileNameStringForMakefile(fileInfo.name)
    end
    
    
    local function createBuildInfoInterface(buildInfo)
        return  {
                    targets = ListInterface(buildInfo.resultFiles, fileInfoInFilter, fileInfoOutFilter),
                    prereqs = ListInterface(buildInfo.prereqFiles, fileInfoInFilter, fileInfoOutFilter)
                }
    end
    
    function interface.Rule(...)
        local arg
        if select("#", ...) == 1 then
            arg = select(1, ...)
            if type(arg) ~= "table" then
                throwError(EXTERNAL, "argument for Rule must be one table or target, prereq and action")
            end
        else
            arg = { ... }
        end
    
        local next = IndexIterator()
    
        local target = arg.target or arg[next()]
        local prereq = arg.prereq or arg[next()]
        local action = arg.action or arg[next()]
    
        if next() == 4 and prereq and not action then
            prereq, action = {}, prereq
        elseif not prereq then
            prereq = {}
        end

        if not target then
            throwError(EXTERNAL, "missing target argument for Rule")
        end
        if type(target) == "table" and #target == 0 then
            throwError(EXTERNAL, "empty target argument for Rule")
        end
        if not action then
            throwError(EXTERNAL, "missing action argument for Rule")
        end
        if type(action) == "string" then
            local shellCmd = action
            action =    function (buildInfo)
                            local firstTarget = fileNameStringForMakefile(buildInfo.resultFiles[1].name)
                            setCurrentDirectory(interfaceWorkingDirectory)
                            local rc = executeAction(shellCmd, interfaceWorkingDirectory,
                                                               createBuildInfoInterface(buildInfo))
                            setCurrentDirectory(initialWorkingDirectory)
                            return rc == 0
                        end
        elseif type(action) == "function" then
            local suppliedAction = action
            action =    function (buildInfo)
                            setCurrentDirectory(interfaceWorkingDirectory)
                            suppliedAction(createBuildInfoInterface(buildInfo))
                            setCurrentDirectory(initialWorkingDirectory)
                        end
        else
            throwError(EXTERNAL, "action argument must be string or function")
        end

        ecall(INTERNAL, core.addRule, checkAndNormalizeTargetList(target, "target"), 
                                      checkAndNormalizeFileList  (prereq, "prereq"), 
                                      action)
    end

    function interface.make(targetNameList)
        return ecall(INTERNAL, core.make, checkAndNormalizeFileList(targetNameList, "targets"))
    end

    function interface.canMake(targetNameList)
        return ecall(INTERNAL, core.canMake, checkAndNormalizeFileList(targetNameList, "targets"))
    end

    return interface
end

return Interface
