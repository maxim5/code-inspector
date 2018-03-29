require 'rational'

class StreamParser
	COMMONR = /Stream #\d+\.\d+\[0x(\d+)\](?:\((\w+)\))?/
	VIDEOR = /#{COMMONR}: Video: (.*?),.*? (\d+x\d+) .*? DAR (\d+:\d+).*?([\d.]+) tb\(r\)/o
	VIDEON = [:pid, :lang, :codec, :size, :dar, :fps]
	AUDIOR = /#{COMMONR}: Audio: (.*?),.*? (\d+) Hz,.*? (5:1|stereo)/o
	AUDION = [:pid, :lang, :codec, :rate, :channels]

	# ffmpeg codec => avchd codec
	CODECS = {
		'h264' => 'H.264',
		'liba52' => 'AC-3',
		'dca' => 'DTS'
	}

	FORMATS = {
		'720x576' => '576i',
		'1920x1080' => '1080p',
		'1280x720' => '720p'
	}

	FPS = {
		'23.98' => '23.976',
		'25.00' => '25'
	}

	CHANNELS = {		
		'5:1' => 'Multi Channel'
	}

	RATES = {
		'48000' => '48 kHz'
	}

	attr_reader :length, :video, :audio

	def initialize filename
		out = `ffmpeg -i "#{filename}" 2>&1`
		unless out =~ /Duration: (\d+):(\d+):([\d,.]+),/
			raise 'Cannot parse ffmpeg output'
		end

		@length = $1.to_i*60*60+$2.to_i*60+$3.to_f

		@video = out.scan(VIDEOR).map do |r|
			tmp = Hash[VIDEON.zip r]
			tmp[:pid] = tmp[:pid].to_i(16)
			tmp[:codec] = CODECS[tmp[:codec]]
			tmp[:format] = FORMATS[tmp[:size]]
			tmp[:fps] = FPS[tmp[:fps]]
			if !VideoStream::ASPECTS[tmp[:dar]] && tmp[:size] =~ /(\d+)x(\d+)/
				x, y = $1.to_i, $2.to_i
				g = x.gcd y
				tmp[:dar] = "%d:%d" % [x/g, y/g]
			end
			tmp
		end
		@audio = out.scan(AUDIOR).map do |r|
			tmp = Hash[AUDION.zip r]
			tmp[:pid] = tmp[:pid].to_i(16)
			tmp[:codec] = CODECS[tmp[:codec]]
			tmp[:format] = CHANNELS[tmp[:channels]]
			tmp[:rate] = RATES[tmp[:rate]]
			tmp
		end
	end
end


# PID
# 0x0100 Program map
# 0x1001 PCR
# 0x1011 Video stream
# 0x1100~0x111F Primary audio stream
# 0x1200~0x121F PG stream
# 0x1400~0x141F IG stream
# 0x1A00~0x1A1F Secondary audio stream

class Stream
	CODECS = {
		"MPEG-1 Video" => 0x01,
		"MPEG-2 Video" => 0x02,
		"MPEG-1 Audio" => 0x03,
		"MPEG-2 Audio" => 0x04,
		"LPCM" => 0x80,
		"AC-3" => 0x81,
		"DTS" => 0x82,
		"TrueHD" => 0x83,
		"AC-3 Plus" => 0x84,
		"DTS-HD" => 0x85,
		"DTS-HD Master" => 0x86,
		"VC-1" => 0xEA,
		"H.264" => 0x1B,
		"Presentation Graphics" => 0x90,
		"Interactive Graphics" => 0x91,
		"Subtitle" => 0x92
	}
	VIDEO_CODECS = [0x01, 0x02, 0xEA, 0x1B]
	AUDIO_CODECS = [0x03, 0x04, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86]
	GRAPH_CODECS = [0x90, 0x91]
	SUB_CODECS = [0x92]

	def video?
		VIDEO_CODECS.include? @coding_type
	end
	def audio?
		AUDIO_CODECS.include? @coding_type
	end
	def graph?
		GRAPH_CODECS.include? @coding_type
	end
	def subtitle?
		SUB_CODECS.include? @coding_type
	end

	attr_reader :coding_type, :format, :rate, :lang, :char_code
end

class VideoStream < Stream
	attr_reader :aspect

	def initialize info
		@coding_type = CODECS[info[:codec]]
		@format = FORMATS[info[:format]]
		@rate = RATES[info[:fps]]
		@aspect = ASPECTS[info[:dar]]
		@lang = info[:lang] || 'und'
		assert @coding_type
		assert @format
		assert @rate
		assert @aspect
		assert @lang
	end

	FORMATS = {
		'480i' => 1,
		'576i' => 2,
		'480p' => 3,
		'1080i' => 4,
		'720p' => 5,
		'1080p' => 6,
		'576p' => 7
	}
	RATES = {
		'23.976' => 1,
		'24' => 2,
		'25' => 3,
		'29.97' => 4,
		'50' => 6,
		'59.94' => 7
	}
	ASPECTS = {
		"4:3" => 2,
		"16:9" => 3
	}

	def pid
		0x1011
	end
end

class AudioStream < Stream
	FORMATS = {
		'Mono' => 1,
		'Stereo' => 3,
		'Multi Channel' => 6,
		'Combo' => 12
	}
	RATES = {
		"48 kHz" => 1,
		"96 kHz" => 4,
		"192 kHz" => 5,
		"48/192 kHz" => 12,
		"48/96 kHz" => 14
	}

	def initialize info
		@coding_type = CODECS[info[:codec]]
		@format = FORMATS[info[:format]]
		@rate = RATES[info[:rate]]
		@lang = info[:lang] || 'und'
		assert @coding_type
		assert @format
		assert @rate
		assert @lang
	end

	def pid
		0x1100
	end
end
