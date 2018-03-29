# #!/data/pmrs/jin/bin/ruby -I /data/pmrs/challis/lib/Atlas-Indel2/trunk/
#
$:.unshift File.join(File.dirname(__FILE__),'lib')
require 'read.rb'
require 'read_indel.rb'
require 'read_insertion.rb'
require 'read_deletion.rb'
require 'assertion_failure.rb'
require 'getoptlong.rb'
require 'ref_seq.rb'
require 'indel.rb'
require 'bed_file.rb'
require 'simple_genotyper.rb'

$VERSION="1.4.3"
$REVISION=158
$DEBUG = false

illum_defaults = { 'site_level_p_cutoff' => 0.5, 'min_var_reads' => 2, 'min_var_ratio' => 0.06, 'strand_dir_filter' => false, 'min_total_depth'=>5 , '1bp_site_level_p_cutoff' => 0.5, 'homo_var_cutoff' => 0.6, 'read_end_ratio' => 0.8, 'orig_base_qual' => false }
solid_defaults = { 'site_level_p_cutoff' => 0.65, 'min_var_reads' => 2, 'min_var_ratio' => 0.05, 'strand_dir_filter' => false, 'min_total_depth'=>2, '1bp_site_level_p_cutoff' => 0.9, 'homo_var_cutoff' => 0.5, 'read_end_ratio' => 1, 'orig_base_qual' => true}

class Atlas_indel2
	# put read level model coefficients here:
	#@@illum_read_intercept = -0.72132
	#@@illum_read_average_nbq = 0.08582
	#@@illum_read_var_rate = -71.42767
	@@illum_site_intercept = -20.5000
	@@illum_site_simple_local_entropy =  3.39065
	@@illum_site_strand_dir = 3.02573
	@@illum_site_norm_var_square = 0.32695
	@@illum_site_mean_ave_nqs = 0.37184
	#@@solid_read_intercept = -4.203181
	#@@solid_read_average_nbq = 0.111440
	#@@solid_read_var_rate = -63.197461
	#@@solid_read_near_end = -0.929674
	@@solid_site_intercept = -5.8931
	@@solid_site_norm_var_square = 0.8200
	@@solid_site_mean_ave_nqs = 0.1144
	@@solid_site_strand_dir = 2.2481
	@@solid_site_var_rate = -53.1852
	@@solid_site_read_end_ratio = -2.4400

	@@read_level_z_cutoff = -1.3
	@@e=2.71828182845904523536
	def initialize(infile, ref_filename, outfile, min_var_reads, min_var_ratio, p_cutoff, p_1bp_cutoff, strand_dir_filter, sample, min_depth_coverage, max_read_end_ratio, model, bed, homo_var_cutoff, orig_base_qual, include_file, show_filtered=false)
		unless(bed.nil?)
			@bed = Bed_file.new(bed)
		end
		@orig_base_qual=orig_base_qual
		if(sample.nil?)
			if(File.basename(infile) =~ /([^\.\/]+)\./)
				@sample = $1
				puts "\"#{@sample}\" will be used as the sample name"
			else
				STDERR.puts "Failed to find a sample name in the input filename.\nPlease specify a sample name with the \"-s [name]\" argument"
				exit 1
			end
		else
			@sample = sample
		end
		begin
			@ref_file = ref_filename
			@reference = Ref_seq.new(ref_filename)
		rescue
			raise "Error loading the reference sequence from file #{ref_filename}:\n#{$!}"
		end
		begin
			@infile = open_file(infile)
			@outfile = File.open(outfile, 'w')
			#	rescue
			#		raise "There was an error loading the input or output file:\n#{$!}"
		end
		@min_var_reads = min_var_reads
		@min_var_ratio = min_var_ratio
		@p_cutoff = p_cutoff
		@p_1bp_cutoff = p_1bp_cutoff
		@strand_dir_filter = strand_dir_filter
		@show_filtered = show_filtered
		@max_read_end_ratio=max_read_end_ratio
		@min_depth_coverage = min_depth_coverage
		if(include_file.nil?)
			@include_sites = []
		else
			@include_sites = load_include_file(include_file)
		end
		@model = model
		@genotyper = Simple_genotyper.new([@min_var_ratio,homo_var_cutoff,0.9,0.5,@min_depth_coverage])
		# require 'getoptlong'
	end

	def self.process_arguments()
		opts = GetoptLong.new(
		["--bam", "-b", GetoptLong::REQUIRED_ARGUMENT],
		["--reference", "-r", GetoptLong::REQUIRED_ARGUMENT ],
		["--outfile","-o", GetoptLong::REQUIRED_ARGUMENT],
		["--sample","-s", GetoptLong::REQUIRED_ARGUMENT],
		["--bed","-B", GetoptLong::REQUIRED_ARGUMENT],
		["--always-include","-a", GetoptLong::REQUIRED_ARGUMENT],
		["--show-filtered","-F", GetoptLong::NO_ARGUMENT],
		["--min-total-depth","-t", GetoptLong::REQUIRED_ARGUMENT],
		["--min-var-reads","-m", GetoptLong::REQUIRED_ARGUMENT],
		["--min-var-ratio","-v", GetoptLong::REQUIRED_ARGUMENT],
		["--p-cutoff","-p", GetoptLong::REQUIRED_ARGUMENT],
		["--p-1bp-cutoff","-P", GetoptLong::REQUIRED_ARGUMENT],
		["--strand-dir-filter","-f", GetoptLong::NO_ARGUMENT],
		["--near-read-end-ratio","-n", GetoptLong::REQUIRED_ARGUMENT],
		["--solid-exome-model","-S", GetoptLong::NO_ARGUMENT],
		["--illumina-exome-model","-I", GetoptLong::NO_ARGUMENT],
		['--homo-var-cutoff', '-h', GetoptLong::REQUIRED_ARGUMENT],
		["--orig-base-qual","-O", GetoptLong::NO_ARGUMENT],
		["--norm-base-qual","-N", GetoptLong::NO_ARGUMENT]
		)

		opt_hash = {}
		begin
			opts.each do |opt, arg|
				opt_hash[opt] = arg
			end
		rescue GetoptLong::InvalidOption
			usage = true
		end

		if(  usage || !(opt_hash.key?("--bam")) || !(opt_hash.key?("--reference")) || !(opt_hash.key?("--outfile")) || (!(opt_hash.key?("--solid-exome-model")) && !(opt_hash.key?("--illumina-exome-model")) ) )
			puts "Atlas-Indel2 v#{$VERSION} r#{$REVISION}"
			puts "USAGE:\n	ruby Atlas-Indel2.rb -b input_bam -r reference_sequence -o outfile [-S or -I]"
			puts "\t\t-S --solid-exome-model (use the model trained for SOLiD exome data)"
			puts "\t\t-I --illumina-exome-model (use the model trained for Illumina exome data)"
			puts "\n\t\toptional arguments:"
			puts "\t\t-s --sample [by default taken from BAM file name] (the name of the sample)"
			puts "\t\t-p --p-cutoff (the p cutoff for the regression model)"
			puts "\t\t-P --p-1bp-cutoff (stricter p cutoff for 1bp indels)"
			puts "\t\t-B --bed"
			puts "\t\t-a --always-include (file of sites with annotation to always include in VCF)"
			puts "\t\t-F --show-filtered (include filtered indels with a QUAL>=1 in the VCF)"
			puts "\t\t-O --orig-base-qual"
			puts "\t\t-N --norm-base-qual"
			puts "\t\t-t --min-total-depth (may not be lower than 4 in Illumina data)"
			puts "\t\t-m --min-var-reads"
			puts "\t\t-v --min-var-ratio"
			puts "\t\t-f --strand-dir-filter (requires at least one read in each direction, extremely limits sensitivity)"
			puts "\t\t-n --near-read-end-ratio (filters indels where more than the specified fraction of indel reads are near the read end)"
			puts "\t\t-h --homo-var-cutoff (homozygous variant cutoff for genotyping)"
			exit 1
		end

		if(opt_hash.key?("--norm-base-qual") && opt_hash.key?('--orig-base-qual'))
			puts "You may not use both the normal base quality and the original base quality.\nPlease specify just one of -O or -N"
			exit 1
		end

		return opt_hash
	end

	def process_data
		puts "Progress:"
		print "loading...\r"
		skipped = 0
		# skipped_flagged = 0
		processed = 0
		processed_chars = 0
		progress = 0
		sec = 0
		buffered_coor = 0 #how far has the current chromosome been buffered?
		print_header
		chr = nil
		indel_buffer = Hash.new
		depth = Hash.new
		ref_depth = Hash.new
		line = get_line()
		@counter=0
		missing_chr=nil
		while(line != :eof)
			begin

				read = Read.new(line, @orig_base_qual)
				unless(@bed.nil?)
					#			if(! @bed.read_included?(read))
					#				raise BedError.new("read is outside of the bed region")
					#			end
				end
				chr = read.ref if chr == nil
				buffered_coor = read.pos - 1 # one less to allow depth coverage to work correctly
				if(indel_buffer.length > 0 && chr == read.ref) # indels are in the buffer, print if possible
					print_buffer(indel_buffer, buffered_coor, chr, depth, ref_depth)
				elsif(chr != read.ref) #reached end of chromosome, print buffer
					print_buffer(indel_buffer, 1.0/0.0, chr, depth, ref_depth) #pass infinity as the buffered_coor to empty the buffer
					assert(indel_buffer.length == 0, "ERROR: Indel buffer is not empty after printing end of chromosome #{chr}. Buffer length: #{indel_buffer.length}\n#{indel_buffer.to_s}")
					indel_buffer = Hash.new
					chr = read.ref
					depth = Hash.new
					ref_depth = Hash.new
				end
				indels = read.get_indels_with_depth(depth, ref_depth) #
				indels.each do |read_indel|
					if(@bed.nil? || @bed.pos_included?(read_indel.chr, read_indel.start_pos)) # skip indels not in the bed file regions (if there is a bed file)
						process_indel(read_indel, indel_buffer)
					end
				end
				processed += 1
			rescue FormatError
				STDERR.puts $!.to_s + "\n\n" + line + "\n\n"
				skipped += 1
			rescue FlaggedError
				# skipped_flagged += 1
				skipped += 1
			rescue BedError
				# skipped_flagged += 1
				skipped += 1
			rescue EORefError
				skipped +=1
				STDERR.puts "Warning: chromosome #{chr} is in the BAM file but not in your reference file, skipping" if missing_chr != chr
				missing_chr=chr
				# rescue
				#	puts "There was an unknown error with a line, it will be skipped.  If you see this message many times you should contact the developer.\n\n#{line}\n\n#{$!}" # skipped += 1
			ensure
				if( Time.now.sec != sec && Time.now.sec % 5 == 0)
					indicate_progress(chr, read.pos)
					sec = Time.now.sec
				end
				line = get_line()
			end
		end
		@infile.close
		print_buffer(indel_buffer, 1.0/0.0, chr, depth, ref_depth)  #print whatever is left in the buffer
		assert(indel_buffer.length == 0, "ERROR: Indel buffer is not empty at the end of the last chromosome. Buffer length: #{indel_buffer.length}\n#{indel_buffer.to_s}")
		while(@include_sites.size()>0)
			print_nonvar_site(@include_sites.shift(), depth, ref_depth)
		end
		@outfile.close
		puts "DONE                                      "
		puts "#{skipped} lines skipped "
		# puts "#{skipped_flagged} flagged lines skipped"
		puts "#{processed} lines processed"
	end

	private

	# loads the file of sites to always include in the VCF
	# sites and annotation stored in a hash of arrays, key is an array with [chr,coor]
	def load_include_file(filename)
		include_sites = Array.new
		File.open(filename, 'r').each_line do |line|
			begin
				cols = line.split("\t")
				chr = cols[0].sub("chr","").to_sym
				coor = cols[1].to_i
				include_sites.push([chr,coor])
			rescue
				STDERR.puts "WARNING: unrecognized line format in sites to include file, skipping line that reads: \"#{line}\""
				next
			end
		end
		return include_sites
	end

	def update_depth(read, chr, depth, ref_depth)
		(read.pos..read.end_pos).each do |coor|
			if(depth[coor] == nil)
				depth[coor] = 1
				ref_depth[coor] = 0
			else
				depth[coor]+= 1
			end
			ref_depth[coor] += 1 if read.seq[coor-read.pos] == @reference.get_base(chr.to_sym, coor)
		end
	end

	def open_file(infile)
		#begin
		if(!File.exist?(infile))
			puts "ERROR: Could not find file: #{infile}"
			exit(1)
		end
		return IO.popen("samtools view -F 1796 #{infile}")
		#rescue
		raise "Error: failed to extract SAM lines from BAM file"
		#end

	end

	def print_header
		@outfile.puts '##fileformat=VCFv4.0'
		@outfile.puts "##fileDate=#{Time.now.strftime("%Y%m%d")}"
		@outfile.puts "##source=Atlas-Indel2 v#{$VERSION} r#{$REVISION}"
		@outfile.puts "##command=\"#{$command}\""
		@outfile.puts "##reference=#{@ref_file}"
		@outfile.puts "##INFO=<ID=P,Number=1,Type=Float,Description=\"Indel p value\">"
		@outfile.puts "##INFO=<ID=ReqIncl,Number=0,Type=Flag,Description=\"Loci is in the list of sites required to be included\">"
		@outfile.puts "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">"
		@outfile.puts "##FORMAT=<ID=GQ,Number=1,Type=Integer,Description=\"Genotype Quality\">"
		@outfile.puts "##FORMAT=<ID=DP,Number=1,Type=Integer,Description=\"Read Depth\">"
		@outfile.puts "##FORMAT=<ID=RR,Number=1,Type=Integer,Description=\"Reference Read Depth\">"
		@outfile.puts "##FORMAT=<ID=VR,Number=1,Type=Integer,Description=\"Major Variant Read Depth\">"
		@outfile.puts "##FILTER=<ID=NonVar,Description=\"No variant at this site\">"
		@outfile.puts "##FILTER=<ID=NoData,Description=\"No sequencing data at this site\">"
		@outfile.puts "##FILTER=<ID=low_qual,Description=\"indel posterior probability is less than #{@p_cutoff}\">"
		@outfile.puts "##FILTER=<ID=1bp_low_qual,Description=\"1bp deletion posterior probability is less than #{@p_1bp_cutoff}\">" if @p_1bp_cutoff != @p_cutoff
		@outfile.puts "##FILTER=<ID=low_VariantReads,Description=\"Number of variant reads is less than #{@min_var_reads}\">"
		@outfile.puts "##FILTER=<ID=low_VariantRatio,Description=\"Variant read ratio is less than #{@min_var_ratio}\">"
		@outfile.puts "##FILTER=<ID=single_strand,Description=\"All variant reads are in the same strand direction\">" if @strand_dir_filter
		@outfile.puts "##FILTER=<ID=low_coverage,Description=\"Total coverage is less than #{@min_depth_coverage}\">"
		@outfile.puts "##FILTER=<ID=read_end_ratio,Description=\"Ratio of variant reads within 5bp of read end is greater than #{@max_read_end_ratio}\">"
		@outfile.puts "#CHROM	POS	ID	REF	ALT	QUAL	FILTER	INFO	FORMAT	#{@sample}"
	end

	def indicate_progress(chr, coor)
		print "Chromosome:#{chr}  Coor:#{coor}                           \r"
		STDOUT.flush
		STDERR.flush
		@outfile.flush
	end

	def process_indel(read_indel, indel_buffer)
		z=nil
		indel_key = read_indel.to_s
		if( indel_buffer[indel_key] == nil)
			indel_buffer[indel_key] = Indel.new(read_indel, z, @@read_level_z_cutoff)
		else
			indel_buffer[indel_key].add_read_indel(z, read_indel)
		end
	end

	def print_buffer(buffer, buffer_coor, chr, depth, ref_depth)
		# print "writing output from buffer..."
		# STDOUT.flush
		i=buffer.length
		prev_indel = nil
		keys = buffer.keys
		keys.sort! if i > 1
		keys.each do |key|
			indel = buffer[key]
			if(!prev_indel.nil?)
				if( indel.read_indel.start_pos == prev_indel.read_indel.start_pos) # check for multiple indels at the same site (we only support one per site)
					if(prev_indel.read_count < indel.read_count)
						prev_indel = indel
					end
					buffer.delete(key)
					next
				else
					print_indel(prev_indel, chr,  depth, ref_depth)
					prev_indel = nil
				end
			end
			if(indel.read_indel.start_pos < buffer_coor)
				buffer.delete(key) # clean out indel buffer
				prev_indel = indel
			end
		end
		print_indel(prev_indel, chr, depth, ref_depth) unless prev_indel.nil?
		while(@include_sites.size() > 0 && chr==@include_sites[0][0] && buffer_coor > @include_sites[0][1])
			print_nonvar_site(@include_sites.shift(), depth, ref_depth)
		end
		depth.delete_if { |coor, depth| coor < buffer_coor } #clean out the depth buffer
		ref_depth.delete_if { |coor, depth| coor < buffer_coor } #clean out the ref_depth buffer
		# print "#{i-buffer.length} lines printed\n"
		# STDOUT.flush
	end

	def print_indel(indel, chr, depth, ref_depth)
		raise "ERROR: missing total depth information for indel #{indel.read_indel.to_s} -- #{indel.read_indel.start_pos()}" if depth[indel.read_indel.start_pos()] == nil
		#assert( chr == key.split(":")[0].to_sym, "ERROR: tried to print #{indel} out on chromosome #{chr}")
		#assert( key.split(/[:DI]/)[1].to_i == indel.read_indel.start_pos, "ERROR: tried to print #{indel} out at wrong coordinate")
		raise "ERROR: at #{chr}:#{indel.read_indel.start_pos()} total depth #{depth[indel.read_indel.start_pos()]} is less than the sum of the reference #{ref_depth[indel.read_indel.start_pos()]} and variant depth#{indel.read_count}" if depth[indel.read_indel.start_pos()] < indel.read_count  + ref_depth[indel.read_indel.start_pos()]
		while(@include_sites.size() > 0 && chr==@include_sites[0][0] && indel.read_indel.start_pos > @include_sites[0][1])
			print_nonvar_site(@include_sites.shift(), depth, ref_depth)
		end
		if(@include_sites.size() > 0 &&  chr==@include_sites[0][0] && indel.read_indel.start_pos == @include_sites[0][1])
			indel.req_include = true
			req_info = ";ReqIncl"
			@include_sites.shift()
		else
			req_info = ""
			
		end
		geno = @genotyper.genotype(ref_depth[indel.read_indel.start_pos()], indel.read_count, depth[indel.read_indel.start_pos()])
		if(indel.read_indel.read.print_chr)
			chr= "chr#{chr}"
		end
		if(@show_filtered || indel.req_include)
			filter, qual, p = filter(indel, depth)

			if(filter == :PASS)
				raise "genotyping error!, passed indel at #{chr}:#{indel.read_indel.start_pos} with a genotype of #{geno}" if(! geno.include?('1'))
				@outfile.puts "#{chr}\t#{indel.read_indel.start_pos}\t.\t#{indel.read_indel.ref_seq}\t#{indel.read_indel.alt_seq}\t#{qual}\tPASS\tP=#{p}#{req_info}\tGT:VR:RR:DP:GQ\t#{geno}:#{indel.read_count}:#{ref_depth[indel.read_indel.start_pos()]}:#{depth[indel.read_indel.start_pos()]}:."

			else
				return if qual < 1 && !indel.req_include # skip extremely low quality indels
				@outfile.puts "#{chr}\t#{indel.read_indel.start_pos}\t.\t#{indel.read_indel.ref_seq}\t#{indel.read_indel.alt_seq}\t#{qual}\t#{filter.join(";")}\tP=#{p}#{req_info}\tGT:VR:RR:DP:GQ\t#{geno}:#{indel.read_count}:#{ref_depth[indel.read_indel.start_pos()]}:#{depth[indel.read_indel.start_pos()]}:."
			end
		else
			qual, p = passes_filters(indel, depth)
			if(qual)
				raise "genotyping error!, passed indel at #{chr}:#{indel.read_indel.start_pos} with a genotype of #{geno}" if(! geno.include?('1'))
				@outfile.puts "#{chr}\t#{indel.read_indel.start_pos}\t.\t#{indel.read_indel.ref_seq}\t#{indel.read_indel.alt_seq}\t#{qual}\tPASS\tP=#{p}#{req_info}\tGT:VR:RR:DP:GQ\t#{geno}:#{indel.read_count}:#{ref_depth[indel.read_indel.start_pos()]}:#{depth[indel.read_indel.start_pos()]}:."
			end
		end
	end

	def print_nonvar_site(include_site, depths, ref_depths)
		if depths[include_site[1]] == nil
			depth = 0
		else
			depth = depths[include_site[1]]
		end
		if ref_depths[include_site[1]] == nil
			ref_depth = 0
		else
			ref_depth = ref_depths[include_site[1]]
		end
		geno = @genotyper.genotype(ref_depth, 0, depth)
		ref_seq = @reference.get_base(include_site[0], include_site[1])
		if(depth>0)
			@outfile.puts "#{include_site[0]}\t#{include_site[1]}\t.\t#{ref_seq}\t.\t.\tNonVar\tP=.;ReqIncl\tGT:VR:RR:DP:GQ\t#{geno}:0:#{ref_depth}:#{depth}:."
		else
			@outfile.puts "#{include_site[0]}\t#{include_site[1]}\t.\t#{ref_seq}\t.\t.\tNoData\tP=.;ReqIncl\tGT:VR:RR:DP:GQ\t#{geno}:0:#{ref_depth}:#{depth}:."
		end
	end

	def read_level_model(average_nbq, var_rate, near_read_end)
		if(@model == :illumina_exome)
			return 0
			#return @@illum_read_intercept + @@illum_read_average_nbq*average_nbq + @@illum_read_var_rate*var_rate
		elsif(@model == :solid_exome)
			if(near_read_end)
				near_read_end = 1.0
			else
				near_read_end = 0.0
			end
			return 0
			#return @@solid_read_intercept + @@solid_read_average_nbq*average_nbq + @@solid_read_var_rate*var_rate + @@solid_read_near_end*near_read_end
		else
			raise "missing or unknown model"
		end
	end

	def get_line
		begin
			line = @infile.readline
			line.chomp!
		rescue EOFError => err
			return :eof
		end
		return line
	end

	def passes_filters(indel, depth)
		return false if indel.read_count < @min_var_reads
		total_depth = depth[indel.read_indel.start_pos()]
		return false if total_depth < @min_depth_coverage
		var_ratio = indel.read_count.to_f / total_depth.to_f
		return false if var_ratio < @min_var_ratio
		return false if @strand_dir_filter && indel.fails_strand_dir_filt
		return false if indel.near_read_end_ratio > @max_read_end_ratio
		z = run_site_model(indel, total_depth)
		p=1/(1+@@e**-z)
		# return filter(s), QUAL, and n
		return false, (p*10000.0).round/10000.0 if p < @p_cutoff || (indel.length ==1 && p < @p_1bp_cutoff)
		return 100, (p*10000.0).round/10000.0 if 1-p < 0.0000000001
		return (-10* Math.log10(1-p)).round, (p*10000.0).round/10000.0
	end

	def filter(indel, depth)
		filters = Array.new
		filters.push "low_VariantReads" if indel.read_count < @min_var_reads
		filters.push 'single_strand' if @strand_dir_filter && indel.fails_strand_dir_filt
		total_depth = depth[indel.read_indel.start_pos()]
		filters.push "low_coverage" if total_depth < @min_depth_coverage
		var_ratio = indel.read_count.to_f / total_depth.to_f
		filters.push "low_VariantRatio"  if var_ratio < @min_var_ratio
		filters.push "read_end_ratio" if indel.near_read_end_ratio > @max_read_end_ratio
		z = run_site_model(indel, total_depth)
		p=1/(1+@@e**-z)
		if p < @p_cutoff
			filters.push "low_qual"
		elsif (indel.is_deletion && indel.length ==1 && p < @p_1bp_cutoff)
			filters.push "1bp_low_qual"
		end
		# return filter(s), QUAL, and p
		if(1-p < 0.0000000001)
			qual = 100
		else
			qual = (-10* Math.log10(1-p)).round
		end
		return :PASS, qual, (p*10000.0).round/10000.0 if filters.length == 0
		return filters, qual, (p*10000.0).round/10000.0
	end

	def run_site_model(indel, total_depth)
		#return 1/(1+@@e**-(
		passed_read_ratio = indel.num_passed_reads.to_f/indel.read_count.to_f
		if(@model == :illumina_exome)
			return @@illum_site_intercept + @@illum_site_simple_local_entropy*indel.simple_local_entropy +
			@@illum_site_strand_dir*indel.strand_dir.to_f + @@illum_site_mean_ave_nqs*indel.mean_avnqs +
			@@illum_site_norm_var_square*(((indel.read_count**2).to_f)/total_depth.to_f)
		elsif(@model == :solid_exome)
			return @@solid_site_intercept + @@solid_site_norm_var_square*((indel.read_count**2).to_f/total_depth.to_f) + @@solid_site_strand_dir*indel.strand_dir.to_f +
			@@solid_site_mean_ave_nqs*indel.mean_avnqs + @@solid_site_var_rate*indel.mean_var_rate + @@solid_site_read_end_ratio*indel.near_read_end_ratio
		else
			raise "specified model: #{@model} not found"
		end
	end

end

# MAIN ##########################
if __FILE__ == $0
	begin
		if(RUBY_VERSION =~ /(\d+)\.(\d+)/)
			if($1.to_i < 1 || $2.to_i < 9)
				STDERR.puts "Atlas-Indel2 requires at least version 1.9 of Ruby, you are running version #{$&}"
				puts "Quitting"
				exit 1
			end
		else
			raise "failed to detect ruby version"
		end
		$command = "#{$0} #{ARGV.join( ' ' )}"

		opt_hash = Atlas_indel2.process_arguments()
		puts "Input BAM: #{opt_hash["--bam"]}"
		puts "Reference Genome: #{opt_hash["--reference"]}"
		puts "Writing output to: #{opt_hash["--outfile"]}"
		sample = opt_hash["--sample"]
		unless( opt_hash["--solid-exome-model"].nil? )
			model = :solid_exome
			defaults = solid_defaults
			puts "Using SOLiD-exome regression model"
		end
		unless( opt_hash["--illumina-exome-model"].nil? )
			model = :illumina_exome
			defaults = illum_defaults
			puts "Using Illumina-exome regression model"
		end
		if(defaults.nil?)
			puts "Please select a sequencing platform"
			exit(1)
		end
		if( opt_hash["--p-cutoff"].nil? )
			p_cutoff = defaults['site_level_p_cutoff']
		else
			p_cutoff = opt_hash["--p-cutoff"].to_f
		end
		puts "Using p cutoff of #{p_cutoff}"
		if( opt_hash["--p-1bp-cutoff"].nil? )
			p_1bp_cutoff = defaults['1bp_site_level_p_cutoff']
		else
			p_1bp_cutoff = opt_hash["--p-1bp-cutoff"].to_f
		end
		puts "Using 1bp deletion p cutoff of #{p_1bp_cutoff}" unless p_1bp_cutoff == p_cutoff
		if( opt_hash["--orig-base-qual"].nil? )
			orig_base_qual = defaults['orig_base_qual']
		else
			orig_base_qual = opt_hash["--orig-base-qual"]
		end
		if( !opt_hash["--norm-base-qual"].nil? )
			orig_base_qual = false
			puts "Using normal base qualities"
		end
		puts "Using original base qualities (if present)" if orig_base_qual
		if(opt_hash["--min-var-reads"].nil?)
			min_var_reads = defaults['min_var_reads']
		else
			min_var_reads = opt_hash["--min-var-reads"].to_i
		end
		puts "Using minimum variant read requirement of #{min_var_reads}"
		if( opt_hash["--min-var-ratio"].nil? )
			min_var_ratio = defaults['min_var_ratio']
		else
			min_var_ratio = opt_hash["--min-var-ratio"].to_f
		end
		puts "Using minimum variant ratio requirement of #{min_var_ratio}"
		if( opt_hash["--min-total-depth"].nil? )
			min_total_depth = defaults['min_total_depth']
		else
			min_total_depth = opt_hash["--min-total-depth"].to_i
		end
		puts "Using minimum total depth requirement of #{min_total_depth}"
		if( opt_hash["--strand-dir-filter"].nil? )
			strand_dir_filter = defaults['strand_dir_filter']
		else
			strand_dir_filter = opt_hash["--strand-dir-filter"]
			puts "Using strand direction filter"
		end
		if( opt_hash["--near-read-end-ratio"].nil? )
			max_read_end_ratio = defaults['read_end_ratio']
		else
			max_read_end_ratio = opt_hash["--near-read-end-ratio"].to_i
		end
		puts "Using maximum near-read-end ratio requirement of #{max_read_end_ratio}" unless max_read_end_ratio == 1
		if( opt_hash['--homo-var-cutoff'].nil?)
			homo_var_cutoff = defaults['homo_var_cutoff']
		else
			homo_var_cutoff = opt_hash['--homo-var-cutoff'].to_f
		end
		puts "Using homozygous variant cutoff of #{homo_var_cutoff} for genotyping"
		unless( opt_hash["--bed"].nil?)
			puts "Only calling indels in the region specified by #{opt_hash["--bed"]}"
		end
		if( opt_hash["--show-filtered"].nil? )
			show_filtered = false
		else
			show_filtered = true
			puts "Showing filtered indels with a QUAL>=1"
		end
		include_file = opt_hash["--always-include"]
		puts "Using sample name #{sample}" unless sample.nil?
		atlas = Atlas_indel2.new(opt_hash["--bam"], opt_hash["--reference"], opt_hash["--outfile"], min_var_reads, min_var_ratio, p_cutoff, p_1bp_cutoff, strand_dir_filter, sample, min_total_depth, max_read_end_ratio, model, opt_hash["--bed"], homo_var_cutoff,  orig_base_qual, include_file, show_filtered)
		atlas.process_data

		print "\nAtlas-INDEL2 Finished\n"
	rescue EORefError
		puts "FATAL ERROR: Reference file error.\n#{$!}"
		exit 1
		#rescue
		#puts "UNKOWN FATAL ERROR: Please contact the developers so they can address this
		# issue (challis@bcm.edu, fyu@bcm.edu).\nPlease include the following
		# details:\n#{$!}"
		#exit 1
	end

end
