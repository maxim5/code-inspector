module.exports = (grunt) ->
    # Load Grunt tasks declared in the package.json file
    require('matchdep').filterDev('grunt-*').forEach(grunt.loadNpmTasks)

    grunt.initConfig
        pkg: @file.readJSON 'package.json'

        app:
            paths:
                base: 'jadfr'
                temp: 'tmp'
                assets: '<%= app.paths.base %>/assets'
                build: '<%= app.paths.assets %>/build'
                bower: '<%= app.paths.temp %>/bower'
                coffee: '<%= app.paths.assets %>/coffee'
                dist: '<%= app.paths.base %>/dist'
                static_src: '<%= app.paths.assets %>/static'
                static_root: '<%= app.paths.base %>/static'
                vendor: '<%= app.paths.static_src %>/vendor'

        bower:
            install:
                options:
                    copy: false
                    cleanBowerDir: false
                    install: true
                    verbose: true

        clean:
            temp: ['<%= app.paths.temp %>']
            build: ['<%= app.paths.build %>/*']
            dist: ['<%= app.paths.dist %>']

        coffee:
            app:
                options:
                    bare: true
                    sourceMap: true
                files: [
                    expand: true
                    cwd: '<%= app.paths.coffee %>'
                    src: ['**/*.coffee']
                    dest: '<%= app.paths.build %>/js'
                    ext: '.js'
                ]

        coffeelint:
            gruntfile: ['Gruntfile.coffee']
            app: [
                '<%= app.paths.coffee %>/**/*.coffee'
            ]
            options:
                no_tabs:
                    level: 'ignore'
                indentation:
                    value: 4
                max_line_length:
                    level: 'warn'
                    value: 150
                no_trailing_semicolons:
                    level: 'warn'

        compass:
            dist:
                options:
                    config: 'compass.rb'
                    environment: 'production'
            build:
                options:
                    config: 'compass.rb'
                    environment: 'development'

        copy:
            vendor:
                files: [{
                    expand: true
                    flatten: true
                    cwd: '<%= app.paths.bower %>',
                    src: [
                        'jquery/dist/jquery*.js',
                        'requirejs/require.js',
                        'requirejs-domready/domReady.js',
                        'requirejs-text/text.js',
                        'holderjs/holder.js',
                        'backbone/*.js',
                        'underscore/*.js',
                        'holderjs/*.js',
                        'backbone.wreqr/lib/backbone.wreqr.*',
                        'backbone.babysitter/lib/backbone.babysitter.*',
                    ]
                    dest: '<%= app.paths.vendor %>'
                    filter: 'isFile'
                }]
            dist:
                files: [{
                    expand: true
                    flatten: false
                    cwd: '<%= app.paths.static_root %>',
                    src: [
                        '**/*',
                    ]
                    dest: '<%= app.paths.dist %>'
                }]

        exec:
            collect_static:
                cwd: '<%= app.paths.base %>'
                command: 'python manage.py collectstatic --noinput'

            manage_py_test:
                cwd: '<%= app.paths.base %>'
                command: 'DJANGO_CONFIGURATION=MyTestConf python manage.py  test'

            pep8:
                cwd: '<%= app.paths.base %>'
                command: 'pep8 --ignore=E501 .'



        jsonlint:
            bower:
                src: ['.bowerrc']
            json:
                src: ['*.json']

        jshint:
            options:
                jshintrc: '.jshintrc'
            app: [
                '<%= app.paths.static_src %>/js/**/*.js'
            ]

        requirejs:
            build:
                options:
                    mainConfigFile: '<%= app.paths.dist %>/js/require.config.js'
                    name: 'main'
                    out: '<%= app.paths.static_root %>/js/main.js'
                    optimize: 'none'
                    optimizeAllPluginResources: true
            dist:
                options:
                    mainConfigFile: '<%= app.paths.static_root %>/js/require.config.js'
                    appDir: '<%= app.paths.static_root %>'
                    baseUrl: 'js'
                    dir: '<%= app.paths.dist %>'
                    paths:
                        'conf': 'empty:'
                    uglify2:
                        compress:
                            sequences: true
                            unused: false
                        warnings: true
                        mangle: true
                    optimize: 'uglify2'
                    optimizeCss: 'none'
                    optimizeAllPluginResources: true
                    removeCombined: true
                    generateSourceMaps: false
                    inlineText: true
                    stubModules: [
                        'text'
                    ]
                    # Look-up all page-modules:
                    # find . -type f -name *.html -print0 | xargs -0 grep -oih -E "{% block requirejs-data-main %}([A-Z0-9\/\-\.]+){% endblock %}"|sed -E 's/.*\%\}([a-zA-Z0-9\/\-\.]+)\{\%.*/\1/g'
                    modules: [{
                        name: 'main'
                        include: [
                            'jquery',
                        ]
                    }, {
                        name: 'I18n'
                        exclude: ['main']
                    }]

        replace:
            projectname:
                src: [
                    '**/*.py',
                    '**/pytest.ini',
                    'compass.rb',
                    'package.json',
                    'Gruntfile.coffee'
                ]
                overwrite: true
                replacements: [{
                    from: /jadfr/g,
                    to: '<%= replace.project %>'
                    }]

        watch:
            json:
                files: [
                    'package.json',
                    '.jshintrc',
                    '*.json'
                ]
                tasks: ['jsonlint', 'jshint:scripts']
            javascript:
                files: ['<%= jshint.app %>']
                tasks: ['jshint:app']
            coffeescript:
                files: ['<%= coffeelint.app %>']
                tasks: ['coffeelint:app', 'coffee:app']
            styles:
                files: ['<%= app.paths.assets %>/sass/**/*.scss']
                tasks: ['compass']
            gruntfile:
                files: ['Gruntfile.coffee']
                tasks: ['coffeelint:gruntfile']
            options:
                spawn: false

    @registerTask 'install', 'Install dependencies managed by Bower.', [
        'bower:install'
        'copy:vendor'
        'clean:temp'
    ]

    @registerTask 'lint', 'Check code quality with static code analysis.', [
        'jsonlint'
        'jshint'
        'coffeelint'
    ]

    @registerTask 'build', 'Lint and compile.', [
        'clean:build'
        'lint'
        'coffee'
        'compass'
    ]

    @registerTask 'dist', 'Build and optimize for distribution.', [
        'clean:dist'
        'build'
        'exec:collect_static'
        # We dont need to call 'copy:dist' as requirejs-optimize copies
        # the whole application directory already.
        'requirejs:dist'
    ]

    @registerTask 'dev', 'Build and watch for changes.', [
        'build'
        'watch'
    ]

    @registerTask 'startproject', 'Add a name for the project', (name) ->
        grunt.fail.warn 'Please enter a project name'  if not name
        grunt.config.set('replace.project', name)
        grunt.task.run 'replace:projectname'

    @registerTask 'test', 'builds the environment and executes ./manage.py test', [
     'build',
     'exec:manage_py_test',
     'exec:pep8',
    ]

    @registerTask 'default', ['dev']
