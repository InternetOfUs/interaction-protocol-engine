/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 1994 - 2021 UDT-IA, IIIA-CSIC
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
package eu.internetofus.wenet_interaction_protocol_engine.api.states;

import static eu.internetofus.common.vertx.HttpResponses.assertThatBodyIs;
import static io.reactiverse.junit5.web.TestRequest.queryParam;
import static io.reactiverse.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.ErrorMessage;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.components.interaction_protocol_engine.StatesPage;
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
   * Should return empty page when the field is undefined.
   *
   * @param query       parameter.
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   */
  @ParameterizedTest(name = " {0}")
  @ValueSource(strings = { "communityId", "taskId", "usetId" })
  public void shouldBeEmptyPageWithUndefinedQueryField(String query, final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    testRequest(client, HttpMethod.GET, States.PATH).with(queryParam(query, "undefined")).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
      final var page = assertThatBodyIs(StatesPage.class, res);
      assertThat(page.total).isEqualTo(0l);
      assertThat(page.states).isNull();

    }).send(testContext);

  }

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
  public void shouldBeEmptyStateForPath(String path, final Vertx vertx, final WebClient client,
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

    var state = new State();
    state.communityId = UUID.randomUUID().toString();
    state.userId = UUID.randomUUID().toString();
    state.attributes.put("key", "value");
    state.attributes.put("key2", UUID.randomUUID().toString());
    var checkpoint = testContext.checkpoint(4);
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

                var newState = new State();
                var newKey2Value = UUID.randomUUID().toString();
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

}
