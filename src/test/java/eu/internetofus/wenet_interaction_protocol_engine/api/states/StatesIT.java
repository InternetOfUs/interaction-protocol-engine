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
package eu.internetofus.wenet_interaction_protocol_engine.api.states;

import static eu.internetofus.common.vertx.HttpResponses.assertThatBodyIs;
import static io.reactiverse.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.model.ErrorMessage;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxTestContext;
import java.util.UUID;
import javax.ws.rs.core.Response.Status;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Integration test over the {@link States}.
 *
 * @see States
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class StatesIT {

  /**
   * Should return empty state for path.
   *
   * @param path        to the community state to get.
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @ParameterizedTest(name = "Get {0} should return empty state")
  @ValueSource(strings = { States.PATH + "/communities/undefined/users/undefined" })
  public void shouldBeEmptyStateForPath(final String path, final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    testRequest(client, HttpMethod.GET, path).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
      final var state = assertThatBodyIs(State.class, res);
      assertThat(state.attributes).isEmpty();

    }).send(testContext);

  }

  /**
   * Should fail merge bad community user JSON.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldFailMergeBadCommunityUserJSON(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    testRequest(client, HttpMethod.PATCH, States.PATH + "/communities/undefined/users/undefined").expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final var error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty();
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);

    }).sendJson(new JsonObject().put("undefined", "undefined"), testContext);

  }

  /**
   * Should merge and get community user state.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldMergeAndGetTwiceCommunityUser(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    final var state = new State();
    state.communityId = UUID.randomUUID().toString();
    state.userId = UUID.randomUUID().toString();
    state.attributes.put("key", "value");
    state.attributes.put("key2", UUID.randomUUID().toString());
    final var checkpoint = testContext.checkpoint(4);
    testRequest(client, HttpMethod.PATCH, States.PATH + "/communities/" + state.communityId + "/users/" + state.userId)
        .expect(resPatch -> {

          assertThat(resPatch.statusCode()).isEqualTo(Status.OK.getStatusCode());
          final var patchedState = assertThatBodyIs(State.class, resPatch);
          assertThat(state.attributes).isEqualTo(patchedState.attributes);

          testRequest(client, HttpMethod.GET,
              States.PATH + "/communities/" + state.communityId + "/users/" + state.userId).expect(resGet -> {

                assertThat(resGet.statusCode()).isEqualTo(Status.OK.getStatusCode());
                final var gettedState = assertThatBodyIs(State.class, resGet);
                assertThat(patchedState).isEqualTo(gettedState);

                final var newState = new State();
                final var newKey2Value = UUID.randomUUID().toString();
                newState.attributes.put("key2", newKey2Value);
                newState.attributes.put("key3", "value3");
                testRequest(client, HttpMethod.PATCH,
                    States.PATH + "/communities/" + state.communityId + "/users/" + state.userId).expect(resPatch2 -> {

                      assertThat(resPatch2.statusCode()).isEqualTo(Status.OK.getStatusCode());
                      final var patchedState2 = assertThatBodyIs(State.class, resPatch2);
                      assertThat(patchedState).isNotEqualTo(patchedState2);
                      patchedState._lastUpdateTs = patchedState2._lastUpdateTs;
                      patchedState.attributes.put("key2", newKey2Value);
                      patchedState.attributes.put("key3", "value3");

                      testRequest(client, HttpMethod.GET,
                          States.PATH + "/communities/" + state.communityId + "/users/" + state.userId)
                              .expect(resGet2 -> {

                                assertThat(resGet2.statusCode()).isEqualTo(Status.OK.getStatusCode());
                                final var gettedState2 = assertThatBodyIs(State.class, resGet2);
                                assertThat(patchedState2).isEqualTo(gettedState2);

                              }).send(testContext, checkpoint);

                    }).sendJson(newState.toJsonObject(), testContext, checkpoint);

              }).send(testContext, checkpoint);

        }).sendJson(state.toJsonObject(), testContext, checkpoint);

  }

  /**
   * Should fail merge bad task user JSON.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldFailMergeBadTaskUserJSON(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    testRequest(client, HttpMethod.PATCH, States.PATH + "/tasks/undefined/users/undefined").expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final var error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty();
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);

    }).sendJson(new JsonObject().put("undefined", "undefined"), testContext);

  }

  /**
   * Should merge and get task user state.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldMergeAndGetTwiceTaskUser(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    final var state = new State();
    state.taskId = UUID.randomUUID().toString();
    state.userId = UUID.randomUUID().toString();
    state.attributes.put("key", "value");
    state.attributes.put("key2", UUID.randomUUID().toString());
    final var checkpoint = testContext.checkpoint(4);
    testRequest(client, HttpMethod.PATCH, States.PATH + "/tasks/" + state.taskId + "/users/" + state.userId)
        .expect(resPatch -> {

          assertThat(resPatch.statusCode()).isEqualTo(Status.OK.getStatusCode());
          final var patchedState = assertThatBodyIs(State.class, resPatch);
          assertThat(state.attributes).isEqualTo(patchedState.attributes);

          testRequest(client, HttpMethod.GET, States.PATH + "/tasks/" + state.taskId + "/users/" + state.userId)
              .expect(resGet -> {

                assertThat(resGet.statusCode()).isEqualTo(Status.OK.getStatusCode());
                final var gettedState = assertThatBodyIs(State.class, resGet);
                assertThat(patchedState).isEqualTo(gettedState);

                final var newState = new State();
                final var newKey2Value = UUID.randomUUID().toString();
                newState.attributes.put("key2", newKey2Value);
                newState.attributes.put("key3", "value3");
                testRequest(client, HttpMethod.PATCH, States.PATH + "/tasks/" + state.taskId + "/users/" + state.userId)
                    .expect(resPatch2 -> {

                      assertThat(resPatch2.statusCode()).isEqualTo(Status.OK.getStatusCode());
                      final var patchedState2 = assertThatBodyIs(State.class, resPatch2);
                      assertThat(patchedState).isNotEqualTo(patchedState2);
                      patchedState._lastUpdateTs = patchedState2._lastUpdateTs;
                      patchedState.attributes.put("key2", newKey2Value);
                      patchedState.attributes.put("key3", "value3");

                      testRequest(client, HttpMethod.GET,
                          States.PATH + "/tasks/" + state.taskId + "/users/" + state.userId).expect(resGet2 -> {

                            assertThat(resGet2.statusCode()).isEqualTo(Status.OK.getStatusCode());
                            final var gettedState2 = assertThatBodyIs(State.class, resGet2);
                            assertThat(patchedState2).isEqualTo(gettedState2);

                          }).send(testContext, checkpoint);

                    }).sendJson(newState.toJsonObject(), testContext, checkpoint);

              }).send(testContext, checkpoint);

        }).sendJson(state.toJsonObject(), testContext, checkpoint);

  }

  /**
   * Should fail merge bad user JSON.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldFailMergeBadUserJSON(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    testRequest(client, HttpMethod.PATCH, States.PATH + "/users/undefined").expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final var error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty();
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);

    }).sendJson(new JsonObject().put("undefined", "undefined"), testContext);

  }

  /**
   * Should merge and get user state.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @Test
  public void shouldMergeAndGetTwiceUser(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    final var state = new State();
    state.userId = UUID.randomUUID().toString();
    state.attributes.put("key", "value");
    state.attributes.put("key2", UUID.randomUUID().toString());
    final var checkpoint = testContext.checkpoint(4);
    testRequest(client, HttpMethod.PATCH, States.PATH + "/users/" + state.userId).expect(resPatch -> {

      assertThat(resPatch.statusCode()).isEqualTo(Status.OK.getStatusCode());
      final var patchedState = assertThatBodyIs(State.class, resPatch);
      assertThat(state.attributes).isEqualTo(patchedState.attributes);

      testRequest(client, HttpMethod.GET, States.PATH + "/users/" + state.userId).expect(resGet -> {

        assertThat(resGet.statusCode()).isEqualTo(Status.OK.getStatusCode());
        final var gettedState = assertThatBodyIs(State.class, resGet);
        assertThat(patchedState).isEqualTo(gettedState);

        final var newState = new State();
        final var newKey2Value = UUID.randomUUID().toString();
        newState.attributes.put("key2", newKey2Value);
        newState.attributes.put("key3", "value3");
        testRequest(client, HttpMethod.PATCH, States.PATH + "/users/" + state.userId).expect(resPatch2 -> {

          assertThat(resPatch2.statusCode()).isEqualTo(Status.OK.getStatusCode());
          final var patchedState2 = assertThatBodyIs(State.class, resPatch2);
          assertThat(patchedState).isNotEqualTo(patchedState2);
          patchedState._lastUpdateTs = patchedState2._lastUpdateTs;
          patchedState.attributes.put("key2", newKey2Value);
          patchedState.attributes.put("key3", "value3");

          testRequest(client, HttpMethod.GET, States.PATH + "/users/" + state.userId).expect(resGet2 -> {

            assertThat(resGet2.statusCode()).isEqualTo(Status.OK.getStatusCode());
            final var gettedState2 = assertThatBodyIs(State.class, resGet2);
            assertThat(patchedState2).isEqualTo(gettedState2);

          }).send(testContext, checkpoint);

        }).sendJson(newState.toJsonObject(), testContext, checkpoint);

      }).send(testContext, checkpoint);

    }).sendJson(state.toJsonObject(), testContext, checkpoint);

  }

}
