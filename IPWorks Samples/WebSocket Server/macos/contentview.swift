import SwiftUI
import IPWorks

struct ContentView: View, WSServerDelegate {
    func onPing(connectionId: Int32, payload: Data, response: Bool) {
        //
    }
    
    @State private var outputRes: String = ""
    @State private var port: String = ""
    
    func onConnected(connectionId: Int32) {
        outputRes += "Client \(connectionId) connected\n"
    }    
    func onDataIn(connectionId: Int32, dataFormat: Int32, text: Data, eom: Bool, eol: Bool) {
        let str = String(decoding: text, as: UTF8.self)
        outputRes += "Incoming data: \(str)\n"
        do {
            try server.sendText(connectionId: connectionId, text: str)
        } catch {
            print(error)
        }
    }  
    func onPing(connectionId: Int32) {}  
    func onLog(connectionId: Int32, logLevel: Int32, message: String, logType: String) {}    
    func onWebSocketOpenRequest(connectionId: Int32, requestURI: String, hostHeader: String, originHeader: String, subProtocols: inout String, extensions: String, requestHeaders: String, statusCode: inout Int32, responseHeaders: inout String) {}    
    func onConnectionRequest(address: String, port: Int32, accept: inout Bool) {}    
    func onDisconnected(connectionId: Int32, statusCode: Int32, description: String) {
        outputRes += "Client \(connectionId) disconnected\n"
    }    
    func onError(connectionId: Int32, errorCode: Int32, description: String) {}    
    func onReadyToSend(connectionId: Int32) {}    
    func onSSLClientAuthentication(connectionId: Int32, certEncoded: Data, certSubject: String, certIssuer: String, status: String, accept: inout Bool) {}    
    func onSSLConnectionRequest(connectionId: Int32, supportedCipherSuites: String, supportedSignatureAlgs: String, certStoreType: inout Int32, certStore: inout String, certPassword: inout String, certSubject: inout String) {}    
    func onSSLStatus(connectionId: Int32, message: String) {}

    var server = WSServer()    
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
          Text("This demo uses the WebSocketServer module to accept incoming connections and echo incoming data to the client via the DataIn event.")
              .padding(.bottom, 5.0)
              .foregroundColor(Color.blue)
          HStack{
              Text("Port:")
              TextField("777", text: $port)
              startButton()
          }
        
        Text("Log:")
        TextEditor(text: $outputRes)
          .border(Color.black, width: 1)
      }
      .padding(.all, 10.0)
      .frame(minWidth: 500)
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
          server.localPort = Int32(port) ?? 777
          if (server.listening)
          {
              try server.setListening(listening: false)
              connected=false
          }
          else
          {
              try server.setListening(listening: true)
              outputRes+="Server listening at:\nws://\(server.localHost):\(String(server.localPort))\n"
              connected=true
          }
        }
        catch
        {
          do
          {
            try server.setListening(listening: false)
            connected=false
          }
          catch {}
          outputRes += "Error: \(error)"
          return
        }
    }, label: {
      Text("\(connectedChange())")
            .font(.system(size: 20)).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
    })
    .buttonStyle(PlainButtonStyle())
  }
    
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
