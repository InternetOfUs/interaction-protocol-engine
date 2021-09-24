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

package eu.internetofus.wenet_interaction_protocol_engine.api.interactions;

import static io.reactiverse.junit5.web.TestRequest.queryParam;
import static io.reactiverse.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.interaction_protocol_engine.Interaction;
import eu.internetofus.common.components.interaction_protocol_engine.InteractionTest;
import eu.internetofus.common.components.interaction_protocol_engine.InteractionsPage;
import eu.internetofus.common.model.Model;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.InteractionsRepository;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.UUID;
import javax.ws.rs.core.Response.Status;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * The integration test over the {@link Interactions}.
 *
 * @see Interactions
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class InteractionsIT {

  /**
   * Should fail get interaction because bad order.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldFailGetInteractionsPageBecauseBadOrder(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    testRequest(client, HttpMethod.GET, Interactions.PATH).with(queryParam("order", "*name")).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());

    }).send(testContext);

  }

  /**
   * Should fail get interaction because bad order.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldFailDeleteInteractionsBecauseNoOneMatchQuery(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    testRequest(client, HttpMethod.DELETE, Interactions.PATH).with(queryParam("appId", UUID.randomUUID().toString()))
        .expect(res -> {

          assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());

        }).send(testContext);

  }

  /**
   * Should fail get interaction because the to is before from.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldFailAddInteractionBecauseBadEncodedValue(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    testRequest(client, HttpMethod.POST, Interactions.PATH).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());

    }).sendJson(new JsonObject().put("undefined", "value"), testContext);

  }

  /**
   * Should add interaction.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldAddInteraction(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    final var interaction = new InteractionTest().createModelExample(1);
    testRequest(client, HttpMethod.POST, Interactions.PATH).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.CREATED.getStatusCode());
      final var body = Model.fromBuffer(res.body(), Interaction.class);
      assertThat(body).isNotNull().isEqualTo(interaction);

    }).sendJson(interaction.toJsonObject(), testContext);

  }

  /**
   * Should fail get interaction because bad order.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldDeleteInteractions(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    final var appId = UUID.randomUUID().toString();
    Future<?> future = Future.succeededFuture();
    for (var i = 0; i < 10; i++) {

      final var interaction = new InteractionTest().createModelExample(i);
      interaction.appId = appId;
      future = future.compose(any -> InteractionsRepository.createProxy(vertx).store(interaction));

    }
    future.onComplete(testContext.succeeding(any -> {

      testRequest(client, HttpMethod.DELETE, Interactions.PATH).with(queryParam("appId", appId)).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.NO_CONTENT.getStatusCode());

      }).send(testContext);

    }));

  }

  /**
   * Should fail get interaction because bad order.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldGetInteractionsPage(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    final var page = new InteractionsPage();
    page.total = 10;
    page.interactions = new ArrayList<>();
    final var appId = UUID.randomUUID().toString();
    Future<?> future = Future.succeededFuture();
    for (var i = 0; i < page.total; i++) {

      final var interaction = new InteractionTest().createModelExample(i);
      interaction.appId = appId;
      future = future.compose(any -> InteractionsRepository.createProxy(vertx).store(interaction).map(stores -> {
        page.interactions.add(interaction);
        return null;
      }));

    }
    future.onComplete(testContext.succeeding(any -> {

      testRequest(client, HttpMethod.GET, Interactions.PATH).with(queryParam("appId", appId)).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
        final var body = Model.fromBuffer(res.body(), InteractionsPage.class);
        assertThat(body).isNotNull().isEqualTo(page);

      }).send(testContext);

    }));

  }

}
