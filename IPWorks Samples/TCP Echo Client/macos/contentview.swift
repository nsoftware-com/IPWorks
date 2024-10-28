import SwiftUI
import IPWorks

struct ContentView: View, TCPClientDelegate {
  func onDataIn(text: Data, eol: Bool) {
    let str = String(decoding: text, as: UTF8.self)
    outputRes += "Incoming data: \(str)"
  }
  func onConnected(statusCode: Int32, description: String) {}
  func onConnectionStatus(connectionEvent: String, statusCode: Int32, description: String) {}
  func onDisconnected(statusCode: Int32, description: String) {}
  func onError(errorCode: Int32, description: String) {}
  func onReadyToSend() {}
  func onSSLServerAuthentication(certEncoded: Data, certSubject: String, certIssuer: String, status: String, accept: inout Bool) {}
  func onSSLStatus(message: String) {}
  
  var client = TCPClient()
  var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
  @State private var server: String = ""
  @State private var port: String = ""
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
    VStack(alignment: .leading)
    {
      Text("This is a demo to show how to connect to a remote echo server, send data, and receive the echoed response. Simply fill in the server and port to connect, then input data you would like to send.")
        .padding(.bottom, 5.0).foregroundColor(Color.blue)
      HStack{
        Text("Server:")
        
        TextField("localhost", text: $server)
        
        
        Text("Port:")
        
        TextField("777", text: $port)
        connectButton()
      }
      
      Group
      {
        HStack{
          
          TextField("Enter message...", text: $message)
          
          sendButton()
        }
        Text("Data received from server:")
        TextEditor(text: $outputRes)
          .border(Color.black, width: 1)
      }
    }
    .padding(.all, 10.0)
    .frame(minWidth: 500)
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
          try client.connectTo(host: server, port: Int32(port) ?? 777)
        }
        connected = client.connected
      }
      catch
      {
        do
        {
          try client.connectTo(host: server, port: Int32(port) ?? 777)
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
        try client.sendLine(text: message)
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
    .buttonStyle(PlainButtonStyle()).disabled(connected == false)
  }
}

struct ContentView_Previews: PreviewProvider {
  static var previews: some View {
    ContentView()
  }
}
