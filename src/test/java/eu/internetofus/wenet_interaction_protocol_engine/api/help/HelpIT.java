/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.help;

import static eu.internetofus.common.vertx.HttpResponses.assertThatBodyIs;
import static io.reactiverse.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxTestContext;
import javax.ws.rs.core.Response.Status;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

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
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldReturnApiInfo(Vertx vertx, final WebClient client, final VertxTestContext testContext) {

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
