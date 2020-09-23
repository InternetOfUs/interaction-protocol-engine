/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.help;

import static eu.internetofus.common.vertx.HttpResponses.assertThatBodyIs;
import static io.vertx.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import javax.ws.rs.core.Response.Status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.http.HttpMethod;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxTestContext;

/**
 * The integration test over the {@link Help}.
 *
 * @see Help
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class HelpIT {

  /**
   * Verify that return the api information.
   *
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldReturnApiInfo(final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.GET, Help.PATH + Help.INFO_PATH).expect(res -> {
      assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
      final var info = assertThatBodyIs(APIInfo.class, res);
      assertThat(info.name).isNotEmpty();
      assertThat(info.apiVersion).isNotEmpty();
      assertThat(info.softwareVersion).isNotEmpty();
      assertThat(info.vendor).isNotEmpty();
      assertThat(info.license).isNotEmpty();
    }).send(testContext);
  }

  /**
   * Verify that return the OpenAPI description.
   *
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldReturnOpenAPiVersion(final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.GET, Help.PATH + Help.OPENAPI_YAML_PATH).expect(res -> {
      assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
      final String body = res.bodyAsString();
      assertThat(body).isNotEmpty();
    }).send(testContext);
  }

}
