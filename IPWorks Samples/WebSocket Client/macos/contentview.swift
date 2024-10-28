import SwiftUI
import IPWorks

struct ContentView: View, WSClientDelegate {
  func onPing(payload: Data, response: Bool) {
    //
  }
  
  func onReadyToSend() {}
  
  func onDataIn(dataFormat: Int32, text: Data, eom: Bool, eol: Bool) {
    let str = String(decoding: text, as: UTF8.self)
    outputRes += "Incoming data: \(str)\n"
  }
  func onPingResponse() {}
  func onHeader(field: String, value: String) {}
  func onLog(logLevel: Int32, message: String, logType: String) {}
  func onRedirect(location: String, accept: inout Bool) {}
  func onSetCookie(name: String, value: String, expires: String, domain: String, path: String, secure: Bool) {}
  func onConnected(statusCode: Int32, description: String) {}
  func onConnectionStatus(connectionEvent: String, statusCode: Int32, description: String) {}
  func onDisconnected(statusCode: Int32, description: String) {}
  func onError(errorCode: Int32, description: String) {}
  func onSSLServerAuthentication(certEncoded: Data, certSubject: String, certIssuer: String, status: String, accept: inout Bool) {}
  func onSSLStatus(message: String) {}
  
  var client = WSClient()
  
  var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
  @State private var server: String = "ws://echo.websocket.events/.ws"
  @State private var message: String = ""
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
      Text("This is a demo to show how to connect to a remote web socket server, send data, and receive the echoed response. Simply fill in the server to connect, then input data you would like to send.")
        .foregroundColor(Color.blue)
      HStack{
        Text("Server:")
        TextField("localhost", text: $server)
        connectButton()
      }
      
      Group
      {
        HStack{
          TextField("Enter message", text: $message)
          sendButton()
        }
        Text("Data recieved from server:")
        TextEditor(text: $outputRes)
          .border(Color.black, width: 1)
      }
    }
    .padding(.all, 10.0)
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
          try client.disconnect()
        }
        else
        {
          try client.connectTo(url: server)
        }
        connected = client.connected
      }
      catch
      {
        do
        {
          try client.connectTo(url: server)
        }
        catch {}
        outputRes += "Error: \(error)"
        return
      }
    }, label: {
      Text("\(connectedChange())")
        .font(.system(size: 20))
        .lineLimit(1)
        .frame(minWidth: 100)
        .background(RoundedRectangle(cornerRadius: 8)
          .fill(Color.gray))
    })
    .buttonStyle(PlainButtonStyle())
  }
  
  @ViewBuilder
  private func sendButton() -> some View {
    Button(action:
            {
      do
      {
        
        try client.sendText(text: message)
        outputRes += "Sent: \(message) to server\n"
      }
      catch
      {
        print(error)
        return
      }
    }, label: {
      Text("Send data")
        .font(.system(size: 20))
        .lineLimit(1)
        .frame(minWidth: 100)
        .background(RoundedRectangle(cornerRadius: 8)
          .fill(Color.gray))
    })
    .buttonStyle(PlainButtonStyle())
    .disabled(connected == false)
  }
}

struct ContentView_Previews: PreviewProvider {
  static var previews: some View {
    ContentView()
  }
}
