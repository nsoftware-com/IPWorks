import SwiftUI
import IPWorks

struct ContentView: View, TCPServerDelegate {
  @State private var outputRes: String = ""
  
  func onConnected(connectionId: Int32, statusCode: Int32, description: String) {
    outputRes += "Client \(connectionId) connected\n"
  }    
  func onConnectionRequest(address: String, port: Int32, accept: inout Bool) {}    
  func onDataIn(connectionId: Int32, text: Data, eol: Bool) {
    let str = String(decoding: text, as: UTF8.self)
    outputRes += "Incoming data: \(str)"
    do {
      try server.sendLine(connectionId: connectionId, text: str)
    } catch {
      print(error)
    }
  }    
  func onDisconnected(connectionId: Int32, statusCode: Int32, description: String) {
    outputRes += "Client \(connectionId) disconnected\n"
  }    
  func onError(connectionId: Int32, errorCode: Int32, description: String) {}    
  func onReadyToSend(connectionId: Int32) {}    
  func onSSLClientAuthentication(connectionId: Int32, certEncoded: Data, certSubject: String, certIssuer: String, status: String, accept: inout Bool) {}    
  func onSSLConnectionRequest(connectionId: Int32, supportedCipherSuites: String, supportedSignatureAlgs: String, certStoreType: inout Int32, certStore: inout String, certPassword: inout String, certSubject: inout String) {}    
  func onSSLStatus(connectionId: Int32, message: String) {}
  
  var server = TCPServer()    
  var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
  @State private var connected = false
  
  func connectedChange() -> String
  {
    if (connected)
    {
      return "Stop Server"
    }
    else
    {
      return "Start Server"
    }
  }
  
  var body: some View {
    VStack(alignment: .center)
    {
      Text("This demo uses the TCPServer module to accept incoming connections and echo incoming data to the client via the DataIn event.").foregroundColor(Color.blue)
      startButton()                
      Text("Data received from client:")
      TextEditor(text: $outputRes)
        .border(Color.black, width: 1)
    }
    .padding(/*@START_MENU_TOKEN@*/.all, 5.0/*@END_MENU_TOKEN@*/)
  }
  
  @ViewBuilder
  private func startButton() -> some View {
    Button(action:
            {
      //client.runtimeLicense = ""
      server.delegate = self
      outputRes = ""
      
      do
      {
        server.localPort = 777
        if (server.listening)
        {
          try server.setListening(listening: false)
        }
        else
        {
          try server.setListening(listening: true)
          outputRes+="Server listening at:\nHost: \(server.localHost)\nPort: \(String(server.localPort))\n"
        }
      }
      catch
      {
        do
        {
          try server.setListening(listening: false)
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
}

struct ContentView_Previews: PreviewProvider {
  static var previews: some View {
    ContentView()
  }
}
