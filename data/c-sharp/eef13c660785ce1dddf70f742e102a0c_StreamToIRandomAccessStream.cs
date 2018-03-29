//Stream ?IRandomAccessStream
//????
byte[] bytes = StreamToBytes(stream);
InMemoryRandomAccessStream memoryStream = new InMemoryRandomAccessStream();
DataWriter datawriter = new DataWriter(memoryStream.GetOutputStreamAt(0));
datawriter.WriteBytes(bytes);
await datawriter.StoreAsync();


//????
var randomAccessStream = new InMemoryRandomAccessStream();
var outputStream = randomAccessStream.GetOutputStreamAt(0);
await RandomAccessStream.CopyAsync(stream.AsInputStream(), outputStream);