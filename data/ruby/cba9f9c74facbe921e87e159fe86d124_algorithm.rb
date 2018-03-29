LOGGER.progname = File.expand_path(__FILE__)

module OpenTox
  module Algorithm 

    class Fminer 

      def self.create_feature_dataset(params)
				LOGGER.debug File.basename(__FILE__) + ": creating feature dataset"
        resource = RestClient::Resource.new(params[:feature_generation_uri], :user => @@users[:users].keys[0], :password => @@users[:users].values[0])
        resource.post :dataset_uri => params[:dataset_uri], :feature_uri => params[:feature_uri]
      end

			def self.uri
				File.join(@@config[:services]["opentox-algorithm"], "fminer")
			end
    end

    class Lazar 
			
			def self.create_model(params)
				LOGGER.debug params
				LOGGER.debug File.basename(__FILE__) + ": creating model"
				LOGGER.debug File.join(@@config[:services]["opentox-algorithm"], "lazar")
        #resource = RestClient::Resource.new(File.join(@@config[:services]["opentox-algorithm"], "lazar"), :user => @@users[:users].keys[0], :password => @@users[:users].values[0], :content_type => "application/x-yaml")
        resource = RestClient::Resource.new(File.join(@@config[:services]["opentox-algorithm"], "lazar"), :user => @@users[:users].keys[0], :password => @@users[:users].values[0], :content_type => "application/x-yaml")
        #@uri = resource.post(:dataset_uri => params[:dataset_uri], :feature_uri => params[:feature_uri], :feature_generation_uri => File.join(@@config[:services]["opentox-algorithm"], "fminer")).chomp
        @uri = resource.post(:dataset_uri => params[:dataset_uri], :prediction_feature => params[:prediction_feature], :feature_generation_uri => File.join(@@config[:services]["opentox-algorithm"], "fminer")).body.chomp
			end

			def self.uri
				File.join(@@config[:services]["opentox-algorithm"], "lazar")
			end

    end

    class Similarity
      def self.weighted_tanimoto(fp_a,fp_b,p)
        common_features = fp_a & fp_b
        all_features = fp_a + fp_b
        common_p_sum = 0.0
        if common_features.size > 0
          common_features.each{|f| common_p_sum += p[f]}
          all_p_sum = 0.0
          all_features.each{|f| all_p_sum += p[f]}
          common_p_sum/all_p_sum
        else
          0.0
        end
      end
    end

  end
end
