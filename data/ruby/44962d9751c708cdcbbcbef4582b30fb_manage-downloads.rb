#manage your downloads folder

require 'fileutils'

path = '/Users/Downloads'
docs = ['.pdf', '.epub', '.txt', '.rtf']
pics = ['.png', '.gif', '.jpg', '.jpeg','.bmp', '.JPEG', '.JPG']
music = ['.mp3', '.wav']
videos = ['.mp4', '.wmv', '.flv', '.mov', '.mpeg']
apps = ['.dmg', '.pkg']
archive = ['.zip', '.tar']

Dir.foreach(path) { |dlfile|
  next if ['.', '..'].include? dlfile
  	FileUtils.mv dlfile, path + '/Books' if docs.include? File.extname(dlfile)
  	FileUtils.mv dlfile, path + '/Pics' if pics.include? File.extname(dlfile)
  	FileUtils.mv dlfile, path + '/Music' if music.include? File.extname(dlfile)
  	FileUtils.mv dlfile, path + '/Videos' if videos.include? File.extname(dlfile)
  	FileUtils.mv dlfile, path + '/Apps' if apps.include? File.extname(dlfile)
  	FileUtils.mv dlfile, path + '/Archive' if archive.include? File.extname(dlfile)
}