import SwiftUI
import IPWorks

struct ContentView: View, SMTPDelegate {
  func onConnectionStatus(connectionEvent: String, statusCode: Int32, description: String) {}
  func onEndTransfer(direction: Int32) {}
  func onError(errorCode: Int32, description: String) {}
  func onExpand(address: String) {}
  func onPITrail(direction: Int32, message: String) {}
  func onSSLServerAuthentication(certEncoded: Data, certSubject: String, certIssuer: String, status: String, accept: inout Bool) {}
  func onSSLStatus(message: String) {}
  func onStartTransfer(direction: Int32) {}
  func onTransfer(direction: Int32, bytesTransferred: Int64, percentDone: Int32, text: Data) {}
  
  var client = SMTP()
  var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
  @State private var host: String = ""
  @State private var port: String = "25"
  @State private var tlsType = 0
  @State private var login: String = ""
  @State private var password: String = ""
  @State private var from: String = ""
  @State private var to: String = ""
  @State private var subject: String = ""
  @State private var plainText: String = ""
  
  func tlsTypeChange(_ tag: Int) {
    if (tag == 2)
    {
      if (port == "25" || port == "587")
      {
        port = "465"
      }
    }
    else if (tag == 1)
    {
      if (port == "25" || port == "465")
      {
        port = "587"
      }
    }
    else
    {
      if (port == "465" || port == "587")
      {
        port = "25"
      }
    }
  }
  
  var body: some View {
    VStack(alignment: .leading)
    {
      Text("This demo uses the SMTP module to send a plain text email. Fill in your SMTP server information, authentication details, and email text, then click send.").foregroundColor(Color.blue)
      HStack(alignment: .top)
      {
        Text("Host:")
        TextField("Enter server host", text: $host)
      }
      
      HStack(alignment: .firstTextBaseline)
      {
        Text("Port:")
        TextField("Enter server port", text: $port)
      }
      
      HStack(alignment: .firstTextBaseline)
      {
        Picker(selection: $tlsType, label: Text("Use TLS")) {
          Text("No").tag(0)
          Text("Explicit").tag(1)
          Text("Implicit").tag(2)
        }
        .pickerStyle(SegmentedPickerStyle())
        .onChange(of: tlsType, perform: tlsTypeChange)
      }
      
      HStack(alignment: .firstTextBaseline)
      {
        Text("Login:")
        TextField("Enter username", text: $login)
      }
      
      HStack(alignment: .firstTextBaseline)
      {
        Text("Password:")
        SecureField("Enter password", text: $password)
      }
      
      Group
      {
        HStack(alignment: .top)
        {
          Text("From:")
          TextField("Enter 'from' address", text: $from)
        }
        
        HStack(alignment: .firstTextBaseline)
        {
          Text("To:")
          TextField("Enter 'to' address", text: $to)
        }
        
        HStack(alignment: .firstTextBaseline)
        {
          Text("Subject:")
          TextField("Enter subject", text: $subject)
        }
        
        Text("Plain text:")
        
        TextEditor(text: $plainText)
          .border(Color.black, width: 1)
        
      }
      sendButton()
    }
    .padding(.all, 8.0)
  }
  
  @ViewBuilder
  private func sendButton() -> some View {
    Button(action:
            {
      //client.runtimeLicense = ""
      client.delegate = self
      
      do
      {
        client.sendTo = to
        client.subject = subject
        client.message = plainText
        
        switch (tlsType) {
        case 1:
          client.sslStartMode = SMTPSSLStartModes.sslExplicit
          break
        case 2:
          client.sslStartMode = SMTPSSLStartModes.sslImplicit
          break
        default:
          client.sslStartMode = SMTPSSLStartModes.sslNone
          break
        }
        
        client.user = login
        client.password = password
        client.mailServer = host
        client.mailPort = Int32(port) ?? 587
        client.from = login
        try client.connect()
        try client.send()
        try client.disconnect()
        print("Message has been sent successfully")
      }
      catch
      {
        print(error)
        do {
          try client.disconnect()
        } catch {}
        return
      }
    }, label: {
      Text("Send")
        .font(.system(size: 20))
        .frame(minWidth: 150,  minHeight: 40)
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
