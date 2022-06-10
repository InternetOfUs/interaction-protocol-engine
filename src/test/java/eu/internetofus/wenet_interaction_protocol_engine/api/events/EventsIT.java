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

package eu.internetofus.wenet_interaction_protocol_engine.api.events;

import static eu.internetofus.common.vertx.HttpResponses.assertThatBodyIs;
import static io.reactiverse.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.interaction_protocol_engine.ProtocolEvent;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolEventTest;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.model.ErrorMessage;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxTestContext;
import javax.ws.rs.core.Response.Status;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * The integration test over the {@link Events}.
 *
 * @see Events
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class EventsIT {

  /**
   * Verify that can send a event.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Events#sendEvent(io.vertx.core.json.JsonObject,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldSendEvent(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    testContext.assertComplete(new ProtocolEventTest().createModelExample(1, vertx, testContext)).onSuccess(event -> {

      testRequest(client, HttpMethod.POST, Events.PATH).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.ACCEPTED.getStatusCode());
        final var sent = assertThatBodyIs(ProtocolEvent.class, res);
        event.id = sent.id;
        assertThat(sent).isEqualTo(event);

      }).sendJson(event.toJsonObject(), testContext);

    });
  }

  /**
   * Verify that can not send a bad event.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Events#sendEvent(io.vertx.core.json.JsonObject,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotSendBadEvent(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.POST, Events.PATH).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final var error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty().isEqualTo("bad_event");
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);

    }).sendJson(new JsonObject().put("key", "value"), testContext);

  }

  /**
   * Verify that can not send an invalid event.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Events#sendEvent(io.vertx.core.json.JsonObject,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotSendInvalidEvent(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    final var event = new ProtocolEventTest().createModelExample(1);
    testRequest(client, HttpMethod.POST, Events.PATH).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final var error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty().isEqualTo("bad_event.appId");
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);

    }).sendJson(event.toJsonObject(), testContext);

  }

  /**
   * Verify that can delete an event.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Events#deleteEvent(long, io.vertx.ext.web.api.service.ServiceRequest,
   *      io.vertx.core.Handler)
   */
  @Test
  public void shouldDeleteEvent(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    testContext.assertComplete(new ProtocolEventTest().createModelExample(1, vertx, testContext)
        .compose(event -> WeNetInteractionProtocolEngine.createProxy(vertx).sendEvent(event))).onSuccess(event -> {

          testRequest(client, HttpMethod.DELETE, Events.PATH + "/" + event.id).expect(res -> {

            assertThat(res.statusCode()).isEqualTo(Status.NO_CONTENT.getStatusCode());

          }).sendJson(event.toJsonObject(), testContext);

        });
  }

}
