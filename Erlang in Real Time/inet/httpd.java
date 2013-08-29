import java.net.*; import java.io.*; import java.util.*;

// Based on TinyHttpd from Niemeyer P, Peck J, Exploring Java, 
// O'Reilly & Associates, 1996, pp 244-245

public class httpd
{
    public static void main(String argv[]) throws IOException
    {
        ServerSocket svrsock = 
            new ServerSocket(Integer.parseInt(argv[0]));
        while (true)
        {
            Socket consock = svrsock.accept();
            new httpdconnection(consock);
        }
    }
}

class httpdconnection extends Thread
{
    Socket sock;

    public httpdconnection(Socket s)
    {
        sock = s;
        setPriority(NORM_PRIORITY - 1);
        start();
    }

    public void run()
    {
        try
        {
            OutputStream out = sock.getOutputStream();
            PrintWriter outw = 
                new PrintWriter(sock.getOutputStream());
            InputStreamReader inr = 
                new InputStreamReader(sock.getInputStream());
            BufferedReader in = new BufferedReader(inr);    
            String req = in.readLine();
            System.out.println("req " + req);
            StringTokenizer st = new StringTokenizer(req);
            if ((st.countTokens() >= 2) && 
                (st.nextToken().equals("GET")))
            {
                req = st.nextToken();
                if (req.startsWith("/"))
                    req = req.substring(1);
                if (req.endsWith("/") || req.equals(""))
                    req = req + "index.html";
                try 
                {
                    FileInputStream fin = 
                        new FileInputStream(req);
                    byte [] data = new byte[fin.available()];
                    fin.read(data);
                    out.write(data);
                }
                catch(FileNotFoundException e)
                {
                    outw.println("404 Not Found");
                }
            }
            else
                outw.println("400 Bad Request");
            sock.close();
        }
        catch (IOException e)
        {
            System.out.println("IO error " + e);
        }
    }
}
