import SwiftUI
import IPWorks

struct ContentView: View, IMAPDelegate {
  func onConnectionStatus(connectionEvent: String, statusCode: Int32, description: String) {}
  func onEndTransfer(direction: Int32) {}
  func onError(errorCode: Int32, description: String) {}
  func onHeader(field: String, value: String) {}
  func onIdleInfo(message: String, cancel: inout Bool) {}
  func onMailboxACL(mailbox: String, user: String, rights: String) {}
  func onMailboxList(mailbox: String, separator: String, flags: String) {}
  func onMessageInfo(messageId: String, subject: String, messageDate: String, from: String, flags: String, size: Int64) {
    mesList += "    Message# \(messageId) \n"
    mesList += "        From: \(from)) \n"
    mesList += "        Subject: \(subject) \n"
    mesList += "        Date: \(messageDate) \n"
    mesList += "        Size: \(size) \n"
  }
  func onMessagePart(partId: String, size: Int64, contentType: String, fileName: String, contentEncoding: String, parameters: String, multipartMode: String, contentId: String, contentDisposition: String) {}
  func onPITrail(direction: Int32, message: String) {
    print(message)
  }
  func onSSLServerAuthentication(certEncoded: Data, certSubject: String, certIssuer: String, status: String, accept: inout Bool) {
    accept = true
  }
  func onSSLStatus(message: String) {}
  func onStartTransfer(direction: Int32) {}
  func onTransfer(direction: Int32, bytesTransferred: Int64, percentDone: Int32, text: String) {}
  
  var client = IMAP()
  var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
  @State private var connected = false
  @State private var host: String = ""
  @State private var port: String = "143"
  @State private var tlsType = 0
  @State private var login: String = ""
  @State private var password: String = ""
  @State private var selectedMailbox: String = ""
  @State private var mesList: String = ""
  
  
  private let mailboxPickerStyle = PopUpButtonPickerStyle()
  
  
  func tlsTypeChange(_ tag: Int) {
    if (tag == 2)
    {
      if (port == "143")
      {
        port = "993"
      }
    }
    else
    {
      if (port == "993")
      {
        port = "143"
      }
    }
  }
  
  var body: some View {
    VStack(alignment: .center)
    {
      Text("Use this demo to connect to an IMAP server and check email. Click the Connect button to login, then select a mailbox and Receive emails.")
        .foregroundColor(Color.blue)
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
        Text("Username:")
        TextField("Enter username", text: $login)
      }
      
      HStack(alignment: .firstTextBaseline)
      {
        Text("Password:")
        SecureField("Enter password", text: $password)
      }
      HStack(alignment: .firstTextBaseline)
      {
        connectButton()
        
        disconnectButton()
      }
      
      Section {
        Picker("Mailboxes", selection: $selectedMailbox) {
          ForEach((0..<client.mailboxList.count), id: \.self) {
            Text(client.mailboxList[Int(Int32($0))].name)
          }
        }
        .pickerStyle(mailboxPickerStyle)
        
      }
      .disabled(connected == false)
      
      receiveButton()
      
      TextEditor(text: $mesList)
        .border(Color.black, width: 1)
    }
    .padding(.all, 8.0)
  }
  
  @ViewBuilder
  private func connectButton() -> some View {
    Button(action:
            {
      //client.runtimeLicense = ""
      client.delegate = self
      do
      {
        switch (tlsType) {
        case 1:
          client.sslStartMode = IMAPSSLStartModes.sslExplicit
          break
        case 2:
          client.sslStartMode = IMAPSSLStartModes.sslImplicit
          break
        default:
          client.sslStartMode = IMAPSSLStartModes.sslNone
          break
        }
        client.user = login
        client.password = password
        client.authMechanism = IMAPAuthMechanisms.amUserPassword
        client.mailServer = host
        client.mailPort = Int32(port) ?? 993
        try client.connect()
      }
      catch
      {
        print(error)
        do {
          try client.disconnect()
        } catch {}
        return
      }
      do
      {
        try client.listMailboxes()
        if (client.mailboxList.count == 0)
        {
        }
        else
        {
          selectedMailbox = "INBOX"
          connected = true
        }
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
      Text("Connect")
        .font(.system(size: 20))
        .frame(minWidth: 150, minHeight: 40)
        .background(RoundedRectangle(cornerRadius: 8)
          .fill(Color.gray))
    })
    .buttonStyle(PlainButtonStyle())
    .disabled(connected == true)
  }
  
  @ViewBuilder
  private func disconnectButton() -> some View {
    Button(action:
            {
      do
      {
        try client.disconnect()
        connected = false
        mesList = ""
      }
      catch
      {
        print(error)
        return
      }
    }, label: {
      Text("Disconnect")
        .font(.system(size: 20))
        .frame(minWidth: 150, minHeight: 40)
        .background(RoundedRectangle(cornerRadius: 8)
          .fill(Color.gray))
    })
    .buttonStyle(PlainButtonStyle())
    .disabled(connected == false)
  }
  
  @ViewBuilder
  private func receiveButton() -> some View{
    Button(action:
            {
      mesList = ""
      do
      {
        client.mailbox = selectedMailbox
        try client.selectMailbox()
        mesList += "Name: \(client.mailbox) \n"
        mesList += "Total Messages: \(client.messageCount) \n"
        mesList += "\nReceiving 10 messages... \n"
        mesList += "\nList of messages: \n"
        client.messageSet = "1:10"
        try client.retrieveMessageInfo()
      }
      catch
      {
        print(error)
        return
      }
    }, label: {
      Text("Receive")
        .font(.system(size: 20))
        .frame(minWidth: 150,  minHeight: 40)
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
