/*
 *    Copyright (c) 2019 Pythian and Valentin Nikotin
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package plutarch.server.config
import java.io.FileNotFoundException
import java.security.{ KeyStore, SecureRandom }
import javax.net.ssl.{ KeyManagerFactory, SSLContext, TrustManagerFactory }
import akka.http.scaladsl.{ ConnectionContext, HttpsConnectionContext }
import scala.util.{ Failure, Success, Try }

object ServerConfig {
  def apply(httpPort: Int = 8080, httpsPort: Int = 9091, ipInterface: String = "0.0.0.0"): Seq[ServerConfig] = Seq(
    new ServerConfig {
      override val port: Int = httpPort
      override val interface: String = ipInterface
      override def connectionContext: Try[ConnectionContext] = Success(ConnectionContext.noEncryption)
    },
    new ServerConfig {
      override def port: Int = httpsPort
      override val interface: String = ipInterface
      override def connectionContext: Try[HttpsConnectionContext] = {
        val password = getTlsKeyStorePassword
        getKeyStore(password)
          .flatMap(keyStore ⇒ getSslContext(password, keyStore))
          .map(sslContext ⇒ ConnectionContext.https(sslContext))
      }
      private def getKeyStore(password: Array[Char]): Try[KeyStore] = {
        val keyStoreFile = "tls/localhost.jks"
        val keystoreInputStream = getClass.getClassLoader.getResourceAsStream(keyStoreFile)
        if (keystoreInputStream == null) {
          Failure(new FileNotFoundException(s"Can't find key store file at $keyStoreFile"))
        } else {
          Try {
            val keyStoreType = "JKS"
            val keyStore = KeyStore.getInstance(keyStoreType)
            keyStore.load(keystoreInputStream, password)
            keyStore
          }
        }
      }
      private def getSslContext(password: Array[Char], keystore: KeyStore): Try[SSLContext] = {
        val keyManagerAlgorithm = "SunX509"
        Try {
          val sslContext = SSLContext.getInstance("TLS")
          val keyManagerFactory = KeyManagerFactory.getInstance(keyManagerAlgorithm)
          keyManagerFactory.init(keystore, password)
          val tmf = TrustManagerFactory.getInstance(keyManagerAlgorithm)
          tmf.init(keystore)
          sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
          sslContext
        }
      }
      private def getTlsKeyStorePassword: Array[Char] = {
        /*val environmentVariableWithPassword = "TLS_KEY_STORE_PASSWORD"
        sys.env.getOrElse(environmentVariableWithPassword, {
          logger.warn("{} is unset", environmentVariableWithPassword)
          "unset"
        }).toCharArray*/
        "9f0ht032fr09fds909SDG$3gt32f#@FDSfs".toCharArray
      }
    })
}

trait ServerConfig {
  def port: Int
  def interface: String
  def connectionContext: Try[ConnectionContext]
}
