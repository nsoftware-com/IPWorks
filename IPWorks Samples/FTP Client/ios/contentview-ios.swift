import SwiftUI
import IPWorks

struct ContentView: View, FTPDelegate {
  func onConnectionStatus(connectionEvent: String, statusCode: Int32, description: String) {}    
  func onEndTransfer(direction: Int32) {}    
  func onError(errorCode: Int32, description: String) {}    
  func onSSLServerAuthentication(certEncoded: Data, certSubject: String, certIssuer: String, status: String, accept: inout Bool) { accept = true }    
  func onSSLStatus(message: String) {}    
  func onStartTransfer(direction: Int32) {}    
  func onTransfer(direction: Int32, bytesTransferred: Int64, percentDone: Int32, text: Data) {}    
  func onDirList(dirEntry: String, fileName: String, isDir: Bool, fileSize: Int64, fileTime: String) {
    outputRes+=fileName+"\n"
  }    
  func onPITrail(direction: Int32, message: String) {}    
  
  var client = FTP()
  var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
  @State private var server: String = ""
  @State private var username: String = ""
  @State private var password: String = ""
  @State private var filename: String = ""
  @State private var outputRes: String = ""
  @State private var connected = false
  
  func connectedChange() -> String
  {
    if (connected)
    {
      return "Disconnect"
    }
    else
    {
      return "Connect"
    }
  }
  
  var body: some View {
    VStack(alignment: .center)
    {
      Text("Enter server details and click 'Connect'. Files can be downloaded/uploaded via the 'Get' and 'Put' buttons.").foregroundColor(Color.blue)
      HStack{
        Text("Server:")
        
        TextField("enter remote host..", text: $server)
          .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
      }
      
      HStack{
        Text("Username:")
        
        TextField("enter username...", text: $username)
          .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
      }
      
      HStack{
        Text("Password:")
        
        SecureField("enter password...", text: $password)
          .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
      }
      
      connectButton()
      
      Group
      {
        HStack{
          Text("Filename:")
          
          TextField("enter remote/local filename...", text: $filename)
            .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
        }
        
        
        HStack()
        {
          downloadButton()
          
          uploadButton()
        }
        
        Text("Remote Files:")
        TextEditor(text: $outputRes)
          .border(Color.black, width: 1)
      }
    }
    .padding(.all, 5.0)
  }
  
  @ViewBuilder
  private func connectButton() -> some View {
    Button(action:
            {
      //client.runtimeLicense = ""
      client.delegate = self
      outputRes = ""      
      do
      {
        if (client.connected == true)
        {
          try client.logoff()
        }
        else
        {
          client.user = username
          client.password = password
          client.remoteHost = server
          try client.logon()
          
          outputRes += "Logged on. Listing directory...\n"
          
          try client.listDirectory()
        }        
        connected = client.connected
      }
      catch
      {
        do
        {
          try client.logoff()
        }
        catch {}
        outputRes += "Error: \(error)"
        return
      }
    }, label: {
      Text("\(connectedChange())")
        .font(.system(size: 20))
        .frame(minWidth: 150, minHeight: 40)
        .background(RoundedRectangle(cornerRadius: 8)
          .fill(Color.gray))
    })
    .buttonStyle(PlainButtonStyle())
  }
  
  @ViewBuilder
  private func downloadButton() -> some View {
    Button(action:
            {
      do
      {
        client.remoteFile = filename
        client.localFile = documentsPath + filename
        try client.download()
        
        print("File successfully downloaded")
      }
      catch
      {
        print(error)
        return
      }
    }, label: {
      Text("Get")
        .font(.system(size: 20))
        .frame(minWidth: 150, minHeight: 40)
        .background(RoundedRectangle(cornerRadius: 8)
          .fill(Color.gray))
    })
    .buttonStyle(PlainButtonStyle())
    .padding(.bottom, 20)
    .disabled(connected == false)
  }
  
  @ViewBuilder
  private func uploadButton() -> some View {
    Button(action:
            {
      outputRes = ""        
      do
      {
        client.localFile = documentsPath + filename
        client.remoteFile = filename
        try client.upload()
        
        try client.listDirectory()
        
        print("File successfully uploaded")
      }
      catch
      {
        print(error)
        return
      }
    }, label: {
      Text("Put")
        .font(.system(size: 20))
        .frame(minWidth: 150, minHeight: 40)
        .background(RoundedRectangle(cornerRadius: 8)
          .fill(Color.gray))
    })
    .buttonStyle(PlainButtonStyle())
    .padding(.bottom, 20)
    .disabled(connected == false)
  }
  
}

struct ContentView_Previews: PreviewProvider {
  static var previews: some View {
    ContentView()
  }
}
