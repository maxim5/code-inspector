Client = require('request-json').JsonClient
should = require 'should'
fixtures = require 'cozy-fixtures'
path = require 'path'

helpers = require './helpers'
helpers.options =
    serverHost: 'localhost'
    serverPort: '8888'
client = new Client "http://#{helpers.options.serverHost}:#{helpers.options.serverPort}/"

fixtures.setDefaultValues
    dirPath: path.resolve __dirname, './fixtures/'
    silent: true
    removeBeforeLoad: false # useless because we clean the DB before tests

describe "Datasystem management", ->

    before helpers.cleanDBWithRequests
    before (done) -> fixtures.load callback: done
    before helpers.startApp
    after helpers.stopApp
    after helpers.cleanDBWithRequests

    describe "When instanciate DataSystem", =>
        before (done) =>
            @CoreClass = require './../server/helpers/CoreClass'
            @dataSystem = require './../server/lib/dataSystem'
            @dataSystem.silent = true
            done()

        it "The CoreClass should exists as a function", =>
            should.exist @CoreClass
            @CoreClass.should.have.type 'function'

        it "The datasystem should exists as an object", =>
            should.exist @dataSystem
            @dataSystem.should.have.type 'object'

        describe "When access to constructor constants", =>

            it "CLASS_NAME should exist as a string", =>
                should.exist @dataSystem.constructor.CLASS_NAME
                @dataSystem.constructor.CLASS_NAME.should.have.type 'string'

        describe "When access to prototype constants", =>

            it "The npm dependency request-json must be load as JSON_CLIENT", =>
                should.exist @dataSystem.JSON_CLIENT
                @dataSystem.JSON_CLIENT.should.have.type 'function'

            it "The local dependency oArrayHelper must be load as ARRAY_HELPER", =>
                should.exist @dataSystem.ARRAY_HELPER
                @dataSystem.ARRAY_HELPER.should.have.type 'object'

            it "Paths constants should be : DS_URL, DS_PORT, DI_PORT", =>
                @dataSystem.should.have.properties 'DS_URL', 'DS_PORT', 'DI_PORT'

            it "Other constants should be : PATH, ERR_MSG", =>
                @dataSystem.should.have.properties 'PATH', 'ERR_MSG'

        describe "When access to instance attributes", =>

            it "The request-json client 'clientDS' should be construct", =>
                should.exist @dataSystem.clientDS
                @dataSystem.clientDS.should.have.type 'object'

            it "The pattern dball paths registery 'registeredPatterns' should be construct", =>
                should.exist @dataSystem.registeredPatterns
                @dataSystem.registeredPatterns.should.have.type 'object'


        describe "When use  CRUD HTTP methods", =>

            describe "When use POST method", =>

                before (done) =>
                    path = @dataSystem.PATH.request + 'alarm' + @dataSystem.PATH.all
                    @dataSystem.postData path, (err, body) =>
                        @errPostValid = err
                        @bodyPostValid = body
                        done()

                it "The valid POST action shouldn't return an error", =>
                    should.not.exist @errPostValid


                it "The valid POST action should return a well formed body", =>
                    should.exist @bodyPostValid
                    @bodyPostValid.should.be.an.Array

                before (done) =>

                    path = @dataSystem.PATH.request + 'fakedoctype' + @dataSystem.PATH.all
                    @dataSystem.postData path, (err, body) =>
                        @errPostFake = err
                        @bodyPostFake = body
                        done()

                it "The not valid POST action should return a 'not found' error", =>
                    should.exist @errPostFake
                    @errPostFake.message.should.equal 'not found'

            describe "When use GET method", =>
                before (done) =>

                    path = @dataSystem.PATH.doctypes.getall
                    @dataSystem.getData path, (err, body) =>
                        @errGetValid = err
                        @bodyGetValid = body
                        done()

                it "The valid GET action shouldn't return an error", =>
                    should.not.exist @errGetValid

                it "The valid GET action should return a well formed body", =>
                    should.exist @bodyGetValid
                    @bodyGetValid.should.be.an.Array

                before (done) =>

                    path = '/fakepath/'
                    @dataSystem.getData path, (err, body) =>
                        @errGetFake = err
                        @bodyGetFake = body
                        done()

                 it "The not valid GET action should return an error", =>
                    should.exist @errGetFake

            describe "When use PUT method", =>
                before (done) =>

                    @pathSumAlarm = '/request/tests/getalarmsum/'
                    viewFunctions =
                        map: (doc) ->
                            if doc.docType? and doc.docType.toLowerCase() is 'alarm'
                                emit doc.docType, 1
                        reduce: (keys, values, rereduce) ->
                            return sum(values)
                    for key, func of viewFunctions
                        viewFunctions[key] = func.toString()

                    @dataSystem.putData @pathSumAlarm, viewFunctions, (err, body) =>
                        @errPut = err
                        @bodyPut = body
                        @dataSystem.postData @pathSumAlarm, (errAlarmSum, bodyAlarmSum) =>
                            @errAlarmSum = errAlarmSum
                            @bodyAlarmSum = bodyAlarmSum
                            done()

                it "The PUT action shouldn't return an error", =>
                should.not.exist @errPut

                it "The PUT action should return a well formed body", =>
                    should.exist @bodyPut
                    @bodyPut.success.should.be.true

                it "After the PUT action, new request must be usable and return sum of alarms", =>
                    should.not.exist @errAlarmSum
                    @bodyAlarmSum[0].value.should.be.equal 2

            describe "When use DELETE method", =>

                before (done) =>
                    path = @dataSystem.PATH.data + @bodyPostValid[1].id + '/'
                    @dataSystem.deleteData path, (err, body) =>
                        @errDel = err
                        @dataSystem.postData @pathSumAlarm, (errAlarmSum, bodyAlarmSum) =>
                            @errAlarmSum2 = errAlarmSum
                            @bodyAlarmSum2 = bodyAlarmSum
                            done()


                it "The DELETE action shouldn't return an error", =>
                    should.not.exist @errDel

                it "After the DELETE action, sum of alarm must be 1", =>
                    should.not.exist @errAlarmSum2
                    @bodyAlarmSum2[0].value.should.be.equal 1

        describe "When use request methods", =>

            describe "When use GET VIEW method", =>

                before (done) =>

                    path = @dataSystem.PATH.application.getpermissions
                    @dataSystem.getView path, (err, body) =>
                        @errView = err
                        @bodyView = body
                        done()


                it "The GET VIEW method shouldn't return an error", =>
                    should.not.exist @errView

                it "The GET VIEW method should return a well formed body", =>
                    should.exist @bodyView
                    @bodyView[0].should.have.keys 'id', 'key', 'value'

            describe "When use GET DOCTYPES method", =>

                before (done) =>
                    @dataSystem.getDoctypes (err, body) =>
                        @errDoctypes = err
                        @bodyDoctypes = body
                        done()

                it "The GET DOCTYPES method shouldn't return an error", =>
                    should.not.exist @errDoctypes

                it "The GET DOCTYPES method should return a well formed body", =>
                    should.exist @bodyDoctypes
                    @bodyDoctypes.should.have.type 'object'

            describe "When use INDEX ID method", =>

                before (done) =>

                    @dataSystem.indexId @bodyPostValid[0].id, ['description'], (err, body) =>
                        @errIndex = err
                        @bodyIndex = body
                        done()

                it "The INDEX ID method shouldn't return an error", =>
                    should.not.exist @errIndex

                it "The INDEX ID method should return a well formed body", =>
                    should.exist @bodyIndex
                    @bodyIndex.success.should.be.true

            describe "When use DELETE ID method", =>

                before (done) =>

                    @dataSystem.deleteById @bodyPostValid[0].id, (err, body) =>
                        @errDelId = err
                        @dataSystem.postData @pathSumAlarm, (errAlarmSum, bodyAlarmSum) =>
                            @errAlarmSum3 = errAlarmSum
                            @bodyAlarmSum3 = bodyAlarmSum
                            done()


                it "The DELETE BY ID method shouldn't return an error", =>
                    should.not.exist @errDeleteId

                it "After DELETE BY ID method, sum of 'alarm' must be 0", =>
                    should.not.exist @errAlarmSum3
                    @bodyAlarmSum3.should.have.lengthOf 0

                 before (done) =>
                    @pathSumMetadoctype = '/request/tests/getmetadoctypesum/'
                    viewFunctions =
                        map: (doc) ->
                            if doc.docType? and doc.docType.toLowerCase() is 'metadoctype'
                                emit doc.docType, 1
                        reduce: (keys, values, rereduce) ->
                            return sum(values)

                    @dataSystem.manageRequest @pathSumMetadoctype, viewFunctions, (err, body) =>
                        @errManageReq = err
                        @bodyManageReq = body
                        @dataSystem.getView @pathSumMetadoctype, (errMDSum, bodyMDSum) =>
                            @errMDSum = errMDSum
                            @bodyMDSum = bodyMDSum
                            done()

            describe "When use DELETE ALL BY DOCTYPES method", =>

                before (done) =>

                    @dataSystem.deleteAllByDoctype 'contact', (err, body) =>
                        @errDelAll = err
                        @bodyDelAll = body
                        requestPath = '/request/' + 'contact' + '/dball/'
                        @dataSystem.getView requestPath, (errAlarmDellAll, bodyAlarmDellAll) =>
                            @errADA = errAlarmDellAll
                            @bodyADA = bodyAlarmDellAll
                            done()

                it "The DELETE ALL BY DOCTYPES method shouldn't return an error", =>
                    should.not.exist @errDelAll

                it "After DELETE BY ID method, sum of 'contact' must be 0", =>
                    should.not.exist @errADA
                    @bodyADA.should.have.lengthOf 0




            describe "When use MANAGE REQUEST method", =>

                it "The MANAGE REQUEST method shouldn't return an error", =>
                    should.not.exist @errManageReq

                it "The MANAGE REQUEST method should return a well formed body", =>
                    should.exist @bodyManageReq
                    @bodyManageReq.success.should.be.true

                it "After the MANAGE REQUEST action, new request must be usable and return sum of metadoctypes", =>
                    should.not.exist @errMDSum
                    @bodyMDSum[0].value.should.be.equal 2

            describe "When use GET DOCTYPES BY ORIGIN method", =>

                before (done) =>

                    @dataSystem.getDoctypesByOrigin (err, body) =>
                        @errOrigin = err
                        @bodyOrigin = body
                        done()

                it "The GET DOCTYPES BY ORIGIN method shouln't return an error", =>
                    should.not.exist @errOrigin

                it "The GET DOCTYPES BY ORIGIN method should return a well formed body", =>
                    should.exist @bodyOrigin
                    @bodyOrigin.should.have.type 'object'

             describe "When use GET DOCTYPES BY APPLICATION method", =>

                before (done) =>

                    @dataSystem.getDoctypesByApplication (err, body) =>
                        @errApp = err
                        @bodyApp = body
                        done()

                it "The GET DOCTYPES BY APPLICATION method shouln't return an error", =>
                    should.not.exist @errApp

                it "The GET DOCTYPES BY APPLICATION method should return a well formed body", =>
                    should.exist @bodyApp
                    @bodyApp.should.have.type 'object'

        describe "When use preparation methods", =>

            describe "When use MANAGE REQUEST method", =>

                before (done) =>
                    aAll =  ['alarm', 'metadoctype']
                    @requestAll = @dataSystem.prepareDballRequests aAll
                    done()

                it "The PREPARE DBALL method should return an array of function", =>
                    @requestAll.should.have.type 'object'
                    @requestAll[0].should.have.type 'function'

        describe "When use format methods", =>

            describe "When use FORMAT BODY method", =>

                before (done) =>
                    fakeBody =
                        rows: []
                    fakeBody.rows.push
                        _id: 'e6dd4fbb424496f74fba2d12e622699a'
                        _rev: '1-ace8aec52e5eee28464753dd4c8c5938'
                        docType: 'alarm'
                        action: 'DISPLAY'
                        trigg: 'Tue Jul 02 2013 16:00:00'
                        description: 'RĂŠunion review scopyleft'
                        related: null
                        id: 'e6dd4fbb424496f74fba2d12e622699a'
                    @newBody = @dataSystem.formatBody fakeBody
                    done()

                it "The FORMAT BODY method should return a well formed body", =>
                    should.exist @newBody
                    @newBody[0].should.have.keys 'id', 'key', 'value'

        describe "When we try to use instance validation methods", =>

            describe "When use ARE VALIDE DOCTYPES method", =>

                before (done) -> fixtures.load callback: done
                before (done) =>
                    doctypes = ['alarm', 'metadoctype']
                    @dataSystem.areValidDoctypes doctypes, (areValid, errMsg) =>
                        @errValid = errMsg
                        @areValid = areValid
                        done()
                before (done) =>
                    doctypes = ['fakedoctype']
                    @dataSystem.areValidDoctypes doctypes, (areValid, errMsg) =>
                        @errValid2 = errMsg
                        @areValid2 = areValid
                        done()

                it "The ARE VALID DOCTYPES method for 'alarm' and 'metadoctype' shouldn't return an error", =>
                    should.not.exist @errValid

                it "The ARE VALID DOCTYPES method for 'alarm' and 'metadoctype' should be true", =>
                    @areValid.should.be.true

                it "The ARE VALID DOCTYPES method for 'fakedoctype' should return an error", =>
                    should.exist @errValid2

                it "The ARE VALID DOCTYPES method for 'fakedoctype' should be false", =>
                    @areValid2.should.be.false





